{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Overloaded plugin, which makes magic possible.
module Overloaded.Plugin (plugin) where

import Control.Monad          (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List              (intercalate)
import Data.List.Split        (splitOn)
import Data.Maybe             (catMaybes)

import qualified Data.Generics as SYB

-- GHC stuff
import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr
import qualified GhcPlugins      as Plugins

import Overloaded.Plugin.Categories
import Overloaded.Plugin.Diagnostics
import Overloaded.Plugin.HasField
import Overloaded.Plugin.IdiomBrackets
import Overloaded.Plugin.LocalDo
import Overloaded.Plugin.Names
import Overloaded.Plugin.Rewrite
import Overloaded.Plugin.V

-------------------------------------------------------------------------------
-- Plugin
-------------------------------------------------------------------------------

-- | @Overloaded@ plugin.
--
-- To enable plugin put the following at top of the module:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols #-}
-- @
--
-- At least one option is required, multiple can given
-- either using multiple @-fplugin-opt@ options, or by separating options
-- with colon:
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Symbols:Numerals #-}
-- @
--
-- Options also take optional desugaring names, for example
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:Labels=Data.Generics.ProductFields.field #-}
-- @
--
-- to desugar @OverloadedLabels@ directly into 'field' from @generics-lens@ (no need to import orphan instance!)
--
-- == Supported options
--
-- * @Symbols@ desugars literal strings to @'Overloaded.Symbols.fromSymbol' \@sym@
-- * @Strings@ works like built-in @OverloadedStrings@ (but you can use different method than 'Data.String.fromString')
-- * @Numerals@ desugars literal numbers to @'Overloaded.Numerals.fromNumeral' \@nat@
-- * @Naturals@ desugars literal numbers to @'Overloaded.Naturals.fromNatural' nat@ (i.e. like 'Data.String.fromString')
-- * @Chars@ desugars literal characters to @'Overloaded.Chars.fromChars' c@. /Note:/ there isn't type-level alternative: we cannot promote 'Char's
-- * @Lists@ __is not__ like built-in @OverloadedLists@, but desugars explicit lists to 'Overloaded.Lists.cons' and 'Overloaded.Lists.nil'
-- * @If@ desugars @if@-expressions to @'Overloaded.If.ifte' b t e@
-- * @Unit@ desugars @()@-expressions to @'Overloaded.Lists.nil'@ (but you can use different method, e.g. @boring@ from <https://hackage.haskell.org/package/boring-0.1.3/docs/Data-Boring.html Data.Boring>)
-- * @Labels@ works like built-in @OverloadedLabels@ (you should enable @OverloadedLabels@ so parser recognises the syntax)
-- * @TypeNats@ and @TypeSymbols@ desugar type-level literals into @'Overloaded.TypeNats.FromNat'@ and @'Overloaded.TypeSymbols.FromTypeSymbol'@ respectively
-- * @Do@ desugar in /Local Do/ fashion. See examples.
-- * @Categories@ change @Arrows@ desugaring to use /"correct"/ category classes.
--
-- == Known limitations
--
-- * Doesn't desugar inside patterns
--
-- == RecordFields
--
-- __WARNING__ the type-checker plugin is experimental, it's adviced to use
--
-- @
-- {-\# OPTIONS_GHC -ddump-simpl #-}
-- @
--
-- to avoid surprising segfaults.
--
-- === Usage
--
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:RecordFields #-}
--
-- === Implementation bits
--
-- See Note [HasField instances] in "ClsInst", the behavior of this plugin is similar.
--
-- The 'GHC.Records.Compat.HasField' class is defined in "GHC.Records.Compat" module of @record-hasfield@ package:
--
-- @
-- class 'GHC.Records.Compat.HasField' {k} x r a | x r -> a where
--     'GHC.Records.Compat.hasField' :: r -> (a -> r, a)
-- @
-- Suppose we have
--
-- @
-- data R y = MkR { foo :: [y] }
-- @
--
-- and @foo@ in scope. We will solve constraints like
--
-- @
-- HasField "foo" (R Int) a
-- @
--
-- by emitting a new wanted constraint
--
-- @
-- [Int] ~# a
-- @
--
-- and building a @HasField@ dictionary out of selector @foo@ appropriately cast.
--
-- == Idiom brackets from TemplateHaskellQuotes
--
-- @
-- {-\# LANGUAGE TemplateHaskellQuotes #-}
-- {-\# OPTIONS_GHC -fplugin=Overloaded -fplugin-opt=Overloaded:IdiomBrackets #-}
--
-- data Tree a
--     = Leaf a
--     | Branch (Tree a) (Tree a)
--   deriving (Show)
--
-- instance Functor Tree where
--     'fmap' f (Leaf x)     = Leaf (f x)
--     'fmap' f (Branch l r) = Branch ('fmap' f l) ('fmap' f r)
--
-- instance Traversable Tree where
--     'traverse' f (Leaf x)     = [| Leaf (f x) |]
--     'traverse' f (Branch l r) = [| Branch ('traverse' f l) ('traverse' f r) |]
-- @
--
plugin :: Plugins.Plugin
plugin = Plugins.defaultPlugin
    { Plugins.renamedResultAction = pluginImpl
    , Plugins.tcPlugin            = enabled tcPlugin
    , Plugins.pluginRecompile     = Plugins.purePlugin
    }
  where
    enabled p args'
        | "RecordFields" `elem` args = Just p
        | otherwise                  = Nothing
      where
        args = concatMap (splitOn ":") args'

pluginImpl
    :: [Plugins.CommandLineOption]
    -> GHC.TcGblEnv
    -> HsGroup GhcRn
    -> GHC.TcM (GHC.TcGblEnv, HsGroup GhcRn)
pluginImpl args' env gr = do
    dflags <- GHC.getDynFlags
    topEnv <- GHC.getTopEnv

    debug $ show args
    debug $ GHC.showPpr dflags gr

    names <- getNames dflags topEnv
    opts@Options {..} <- parseArgs dflags args
    when (opts == defaultOptions) $
        warn dflags noSrcSpan $ GHC.text "No Overloaded features enabled"

    let transformNoOp :: a -> Rewrite a
        transformNoOp _ = NoRewrite

    trStr <- case optStrings of
        NoStr         -> return transformNoOp
        Str Nothing   -> return $ transformStrings names
        Sym Nothing   -> return $ transformSymbols names
        Str (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformStrings $ names { fromStringName = n }
        Sym (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformSymbols $ names { fromSymbolName = n }

    trNum <- case optNumerals of
        NoNum         -> return transformNoOp
        IsNum Nothing   -> return $ transformNumerals names
        IsNat Nothing   -> return $ transformNaturals names
        IsNum (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformNumerals $ names { fromNumeralName = n }
        IsNat (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformNaturals $ names { fromNaturalName = n }

    trChr <- case optChars of
        Off        -> return transformNoOp
        On Nothing -> return $ transformChars names
        On (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformChars $ names { fromCharName = n }

    trLists <- case optLists of
        Off        -> return transformNoOp
        On Nothing -> return $ transformLists names
        On (Just (V2 xn yn)) -> do
            x <- lookupVarName dflags topEnv xn
            y <- lookupVarName dflags topEnv yn
            return $ transformLists $ names { nilName = x, consName = y }

    trIf <- case optIf of
        Off        -> return transformNoOp
        On Nothing -> return $ transformIf names
        On (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformIf $ names { ifteName = n }

    trLabel <- case optLabels of
        Off          -> return transformNoOp
        On Nothing   -> return $ transformLabels names
        On (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformLabels $ names { fromLabelName = n }

    trCodeLabel <- case optCodeLabels of
        Off          -> return transformNoOp
        On Nothing   -> return $ transformCodeLabels names
        On (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformCodeLabels $ names { codeFromLabelName = n }

    trBrackets <- case optIdiomBrackets of
        False -> return transformNoOp
        True  -> return $ transformIdiomBrackets names

    trDo <- case optDo of
        False -> return transformNoOp
        True  -> return $ transformDo names

    trCategories <- case optCategories of
        Off          -> return transformNoOp
        On Nothing   -> return $ transformCategories names
        On (Just mn) -> do
            catNames' <- getCatNames dflags topEnv (GHC.mkModuleName mn)
            return $ transformCategories $ names { catNames = catNames' }

    trUnit <- case optUnit of
        Off        -> return transformNoOp
        On Nothing -> return $ transformUnit names
        On (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformUnit $ names { unitName = n }

    trTypeNats <- case optTypeNats of
        Off          -> return transformNoOp
        On Nothing   -> return $ transformTypeNats names
        On (Just vn) -> do
            n <- lookupTypeName dflags topEnv vn
            return $ transformTypeNats $ names { fromTypeNatName = n }

    trTypeSymbols <- case optTypeSymbols of
        Off          -> return transformNoOp
        On Nothing   -> return $ transformTypeSymbols names
        On (Just vn) -> do
            n <- lookupTypeName dflags topEnv vn
            return $ transformTypeSymbols $ names { fromTypeSymbolName = n }

    let tr  = mconcat
            [ trStr
            , trNum
            , trChr
            , trLists
            , trIf
            , trLabel
            , trCodeLabel
            , trBrackets
            , trDo
            , trCategories
            , trUnit
            ]
    let trT = trTypeNats <> trTypeSymbols

    gr' <- transformType dflags trT gr
    gr'' <- transform dflags tr gr'

    return (env, gr'')
  where
    args = concatMap (splitOn ":") args'

-------------------------------------------------------------------------------
-- Args parsing
-------------------------------------------------------------------------------

parseArgs :: forall m. MonadIO m => GHC.DynFlags -> [String] -> m Options
parseArgs dflags args0 = foldM go0 defaultOptions args0 >>= check where
    check :: Options -> m Options
    check opts = do
        when (isOn (optLabels opts) && isOn (optCodeLabels opts)) $
            warn dflags noSrcSpan $
                GHC.text "Overloaded:Strings and Overloaded:Symbols enabled"
                GHC.$$
                GHC.text "picking Overloaded:Strings"

        return opts

    go0 opts arg = do
        (arg', vns) <- elaborateArg arg
        go opts arg' vns

    go opts "Strings" vns = do
        when (isSym $ optStrings opts) $ warn dflags noSrcSpan $
            GHC.text "Overloaded:Strings and Overloaded:Symbols enabled"
            GHC.$$
            GHC.text "picking Overloaded:Strings"

        mvn <- oneName "Strings" vns
        return $ opts { optStrings = Str mvn }

    go opts "Symbols" vns = do
        when (isStr $ optStrings opts) $ warn dflags noSrcSpan $
            GHC.text "Overloaded:Strings and Overloaded:Symbols enabled"
            GHC.$$
            GHC.text "picking Overloaded:Symbols"

        mvn <- oneName "Symbols" vns
        return $ opts { optStrings = Sym mvn }

    go opts "Numerals" vns = do
        when (isNat $ optNumerals opts) $ warn dflags noSrcSpan $
            GHC.text "Overloaded:Numerals and Overloaded:Naturals enabled"
            GHC.$$
            GHC.text "picking Overloaded:Numerals"

        mvn <- oneName "Numerals" vns
        return $ opts { optNumerals = IsNum mvn }

    go opts "Naturals" vns = do
        when (isNum $ optNumerals opts) $ warn dflags noSrcSpan $
            GHC.text "Overloaded:Numerals and Overloaded:Naturals enabled"
            GHC.$$
            GHC.text "picking Overloaded:Naturals"

        mvn <- oneName "Naturals" vns
        return $ opts { optNumerals = IsNat mvn }

    go opts "Chars"    vns = do
        mvn <- oneName "Chars" vns
        return $ opts { optChars = On mvn }
    go opts "Lists"    vns = do
        mvn <- twoNames "Lists" vns
        return $ opts { optLists = On mvn }
    go opts "If"       vns = do
        mvn <- oneName "If" vns
        return $ opts { optIf = On mvn }
    go opts "Unit"       vns = do
        mvn <- oneName "Unit" vns
        return $ opts { optUnit = On mvn }
    go opts "Labels"   vns = do
        mvn <- oneName "Labels" vns
        return $ opts { optLabels = On mvn }
    go opts "CodeLabels" vns = do
        mvn <- oneName "CodeLabels" vns
        return $ opts { optCodeLabels = On mvn }
    go opts "TypeNats" vns = do
        mvn <- oneName "TypeNats" vns
        return $ opts { optTypeNats = On mvn }
    go opts "TypeSymbols" vns = do
        mvn <- oneName "TypeSymbols" vns
        return $ opts { optTypeSymbols = On mvn }
    go opts "RecordFields" _ =
        return $ opts { optRecordFields = True }
    go opts "IdiomBrackets" _ =
        return $ opts { optIdiomBrackets = True }
    go opts "Do" _ =
        return $ opts { optDo = True }
    go opts "Categories" vns = do
        mvn <- oneName "Categories" vns
        return $ opts { optCategories = On $ fmap (\(VN x _) -> x) mvn }

    go opts s _ = do
        warn dflags noSrcSpan $ GHC.text $ "Unknown Overloaded option " ++  show s
        return opts

    oneName :: [Char] -> [a] -> m (Maybe a)
    oneName arg vns = case vns of
        []     -> return Nothing
        [vn]   -> return (Just vn)
        (vn:_) -> do
            warn dflags noSrcSpan $ GHC.text $ "Multiple desugaring names specified for " ++ arg
            return (Just vn)

    twoNames arg vns = case vns of
        []  -> return Nothing
        [_] -> do
            warn dflags noSrcSpan $ GHC.text $ "Only single desugaring name specified for " ++ arg
            return Nothing
        [x,y]   -> return (Just (V2 x y))
        (x:y:_) -> do
            warn dflags noSrcSpan $ GHC.text $ "Over two names specified for " ++ arg
            return (Just (V2 x y))

    elaborateArg :: String -> m (String, [VarName])
    elaborateArg s = case splitOn "=" s of
        []       -> return ("", [])
        (pfx:xs) -> do
            vns <- traverse parseVarName xs
            return (pfx, catMaybes vns)

    parseVarName :: String -> m (Maybe VarName)
    parseVarName "" = return Nothing
    parseVarName xs = do
        let ps = splitOn "." xs
        return (Just (VN (intercalate "." $ init ps) (last ps)))

data Options = Options
    { optStrings       :: StrSym
    , optNumerals      :: NumNat
    , optChars         :: OnOff VarName
    , optLists         :: OnOff (V2 VarName)
    , optIf            :: OnOff VarName
    , optLabels        :: OnOff VarName
    , optCodeLabels    :: OnOff VarName
    , optUnit          :: OnOff VarName
    , optTypeNats      :: OnOff VarName
    , optTypeSymbols   :: OnOff VarName
    , optRecordFields  :: Bool
    , optIdiomBrackets :: Bool
    , optDo            :: Bool
    , optCategories    :: OnOff String -- module name
    }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optStrings       = NoStr
    , optNumerals      = NoNum
    , optChars         = Off
    , optLists         = Off
    , optIf            = Off
    , optLabels        = Off
    , optCodeLabels    = Off
    , optTypeNats      = Off
    , optTypeSymbols   = Off
    , optUnit          = Off
    , optRecordFields  = False
    , optIdiomBrackets = False
    , optDo            = False
    , optCategories    = Off
    }

data StrSym
    = NoStr
    | Str (Maybe VarName)
    | Sym (Maybe VarName)
  deriving (Eq, Show)

isSym :: StrSym -> Bool
isSym (Sym _) = True
isSym _       = False

isStr :: StrSym -> Bool
isStr (Str _) = True
isStr _       = False

data NumNat
    = NoNum
    | IsNum (Maybe VarName)
    | IsNat (Maybe VarName)
  deriving (Eq, Show)

isNum :: NumNat -> Bool
isNum (IsNum _) = True
isNum _       = False

isNat :: NumNat -> Bool
isNat (IsNat _) = True
isNat _         = False

data OnOff a
    = Off
    | On (Maybe a)
  deriving (Eq, Show)

isOn :: OnOff a -> Bool
isOn (On _) = True
isOn Off    = False

-------------------------------------------------------------------------------
-- OverloadedStrings
-------------------------------------------------------------------------------

transformStrings :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformStrings Names {..} e@(L l (HsLit _ (HsString _ _fs))) =
    Rewrite $ hsApps l (hsVar l fromStringName) [e]

transformStrings _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedSymbols
-------------------------------------------------------------------------------

transformSymbols :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformSymbols Names {..} (L l (HsLit _ (HsString _ fs))) = do
    let name' = hsVar l fromSymbolName
    let inner = hsTyApp l name' (HsTyLit noExtField (HsStrTy GHC.NoSourceText fs))
    Rewrite inner

transformSymbols _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedNumerals
-------------------------------------------------------------------------------

transformNumerals :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformNumerals Names {..} (L l (HsOverLit _ (OverLit _ (HsIntegral (GHC.IL _ n i)) _)))
    | not n, i >= 0 = do
        let name' = hsVar l fromNumeralName
        let inner = hsTyApp l name' (HsTyLit noExtField (HsNumTy GHC.NoSourceText i))
        Rewrite inner

transformNumerals _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedNaturals
-------------------------------------------------------------------------------

transformNaturals :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformNaturals Names {..} e@(L l (HsOverLit _ (OverLit _ (HsIntegral (GHC.IL _ n i)) _)))
    | not n
    , i >= 0
    = Rewrite $ hsApps l (hsVar l fromNaturalName) [e]

transformNaturals _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedChars
-------------------------------------------------------------------------------

transformChars :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformChars Names {..} e@(L l (HsLit _ (HsChar _ _))) =
    Rewrite $ hsApps l (hsVar l fromCharName) [e]

transformChars _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedLists
-------------------------------------------------------------------------------

transformLists :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformLists Names {..} (L l (ExplicitList _ Nothing xs)) =
    Rewrite $ foldr cons' nil' xs
  where
    cons' :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
    cons' y ys = hsApps l (hsVar l consName) [y, ys]

    nil' :: LHsExpr GhcRn
    nil' = hsVar l nilName

    -- otherwise: leave intact
transformLists _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedIf
-------------------------------------------------------------------------------

transformIf :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformIf Names {..} (L l (HsIf _ _ co th el)) = Rewrite val4 where
    val4 = L l $ HsApp noExtField val3 el
    val3 = L l $ HsApp noExtField val2 th
    val2 = L l $ HsApp noExtField val1 co
    val1 = L l $ HsVar noExtField $ L l ifteName
transformIf _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedLabels
-------------------------------------------------------------------------------

transformLabels :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformLabels Names {..} (L l (HsOverLabel _ Nothing fs)) = do
    let name' = hsVar l fromLabelName
    let inner = hsTyApp l name' (HsTyLit noExtField (HsStrTy GHC.NoSourceText fs))
    Rewrite inner

transformLabels _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedCodeLabels
-------------------------------------------------------------------------------

transformCodeLabels :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformCodeLabels Names {..} (L l (HsOverLabel _ Nothing fs)) = do
    let name' = hsVar l codeFromLabelName
    let inner = hsTyApp l name' (HsTyLit noExtField (HsStrTy GHC.NoSourceText fs))
    -- Rewrite $ L l $ HsRnBracketOut noExtField (ExpBr noExtField inner) []
    WithName $ \n -> Rewrite $ L l $ HsSpliceE noExtField $ HsTypedSplice noExtField HasParens n inner

transformCodeLabels _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedUnit
-------------------------------------------------------------------------------

transformUnit :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformUnit Names {..} (L l (HsVar _ (L _ name')))
    | name' == ghcUnitName = Rewrite (hsVar l unitName)
  where
    ghcUnitName = GHC.getName (GHC.tupleDataCon GHC.Boxed 0)

transformUnit _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedTypeNats
-------------------------------------------------------------------------------

transformTypeNats :: Names -> LHsType GhcRn -> Rewrite (LHsType GhcRn)
transformTypeNats Names {..} e@(L l (HsTyLit _ (HsNumTy _ _))) = do
    let name' = L l $ HsTyVar noExtField NotPromoted $ L l fromTypeNatName
    Rewrite $ L l $ HsAppTy noExtField name' e
transformTypeNats _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedTypeSymbols
-------------------------------------------------------------------------------

transformTypeSymbols :: Names -> LHsType GhcRn -> Rewrite (LHsType GhcRn)
transformTypeSymbols Names {..} e@(L l (HsTyLit _ (HsStrTy _ _))) = do
    let name' = L l $ HsTyVar noExtField NotPromoted $ L l fromTypeSymbolName
    Rewrite $ L l $ HsAppTy noExtField name' e
transformTypeSymbols _ _ = NoRewrite

-------------------------------------------------------------------------------
-- Transform
-------------------------------------------------------------------------------

transform
    :: GHC.DynFlags
    -> (LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn))
    -> HsGroup GhcRn
    -> GHC.TcM (HsGroup GhcRn)
transform dflags f = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcRn -> GHC.TcM (LHsExpr GhcRn)
    transform' e@(L l _) = do
        -- liftIO $ GHC.putLogMsg _dflags GHC.NoReason GHC.SevWarning _l (GHC.defaultErrStyle _dflags) $
        --     GHC.text "Expr" GHC.<+> GHC.ppr e GHC.<+> GHC.text (SYB.gshow e)
        go (f e)
      where
        go NoRewrite    = return e
        go (Rewrite e') = return e'
        go (Error err)  = do
            liftIO $ err dflags
            fail "Error in Overloaded plugin"
        go (WithName kont) = do
            n <- GHC.newNameAt (GHC.mkVarOcc "olSplice") l
            go (kont n)

transformType
    :: GHC.DynFlags
    -> (LHsType GhcRn -> Rewrite (LHsType GhcRn))
    -> HsGroup GhcRn
    -> GHC.TcM (HsGroup GhcRn)
transformType dflags f = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsType GhcRn -> GHC.TcM (LHsType GhcRn)
    transform' e@(L l _) = go (f e)
      where
        go NoRewrite    = return e
        go (Rewrite e') = return e'
        go (Error err)  = do
            liftIO $ err dflags
            fail "Error in Overloaded plugin"
        go (WithName kont) = do
            n <- GHC.newNameAt (GHC.mkVarOcc "olSplice") l
            go (kont n)
