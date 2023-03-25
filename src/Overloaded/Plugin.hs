{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Overloaded plugin, which makes magic possible.
module Overloaded.Plugin (plugin) where

import Control.Exception      (throwIO)
import Control.Monad          (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List              (intercalate)
import Data.List.Split        (splitOn)
import Data.Maybe             (catMaybes)

import qualified Data.Generics as SYB

-- GHC stuff
import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Plugins as Plugins
#else
import qualified GhcPlugins as Plugins
#endif

import Overloaded.Plugin.Categories
import Overloaded.Plugin.Diagnostics
import Overloaded.Plugin.IdiomBrackets
import Overloaded.Plugin.LocalDo
import Overloaded.Plugin.Names
import Overloaded.Plugin.Rewrite
import Overloaded.Plugin.TcPlugin
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
-- * @CodeLabels@ desugars @OverloadedLabels@ into Typed Template Haskell splices
-- * @CodeStrings@ desugars string literals into Typed Template Haskell splices
-- * @RebindableApplication@ changes how juxtaposition is interpreted
-- * @OverloadedConstructors@ allows you to use overloaded constructor names!
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
-- == RebindableApplication
--
-- Converts all @f x@ applications into @(f $ x)@ with whatever @$@
-- is in scope.
--
-- @
-- {-\# OPTIONS -fplugin=Overloaded -fplugin-opt=Overloaded:RebindableApplication #-}
--
-- let f = pure ((+) :: Int -> Int -> Int)
--     x = Just 1
--     y = Just 2
--
--     z = let ($) = ('<*>') in f x y
-- in z
-- @
--
plugin :: Plugins.Plugin
plugin = Plugins.defaultPlugin
    { Plugins.renamedResultAction = renamedAction
    , Plugins.parsedResultAction  = parsedAction
    , Plugins.tcPlugin            = enabled tcPlugin
    , Plugins.pluginRecompile     = Plugins.purePlugin
    }
  where
    enabled p args'
        | "RecordFields" `elem` args = Just p
        | "Constructors" `elem` args = Just p
        | otherwise                  = Nothing
      where
        args = map (takeWhile (/= '=')) $ concatMap (splitOn ":") args'

-------------------------------------------------------------------------------
-- Renamer
-------------------------------------------------------------------------------

renamedAction
    :: [Plugins.CommandLineOption]
    -> GHC.TcGblEnv
    -> HsGroup GhcRn
    -> GHC.TcM (GHC.TcGblEnv, HsGroup GhcRn)
renamedAction args' env gr = do
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
        NoStr             -> return transformNoOp
        Str Nothing       -> return $ transformStrings names
        Sym Nothing       -> return $ transformSymbols names
        CodeStr Nothing   -> return $ transformCodeStrings names
        Str (Just vn)     -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformStrings $ names { fromStringName = n }
        Sym (Just vn)     -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformSymbols $ names { fromSymbolName = n }
        CodeStr (Just vn) -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformSymbols $ names { codeFromStringName = n }

    trNum <- case optNumerals of
        NoNum           -> return transformNoOp
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
        NoLabel                 -> return transformNoOp
        Label Nothing       -> return $ transformLabels names
        CodeLabel Nothing   -> return $ transformCodeLabels names
        Label (Just vn)     -> do
            n <- lookupVarName dflags topEnv vn
            return $ transformLabels $ names { fromLabelName = n }
        CodeLabel (Just vn) -> do
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
            , trBrackets
            , trDo
            , trCategories
            , trUnit
            ]
    let trT = trTypeNats <> trTypeSymbols

    gr' <- transformType dflags trT gr
    gr'' <- transformRn dflags tr gr'

    return (env, gr'')
  where
    args = concatMap (splitOn ":") args'

-------------------------------------------------------------------------------
-- Parsed Action
-------------------------------------------------------------------------------

parsedAction
    :: [Plugins.CommandLineOption]
    -> Plugins.ModSummary
    -> GHC.HsParsedModule
    -> Plugins.Hsc GHC.HsParsedModule
parsedAction args _modSum pm = do
    let hsmodule = GHC.hpm_module pm
    topEnv <- GHC.Hsc $ \env warnMsgs -> return (env, warnMsgs)

    dflags <- GHC.getDynFlags

    debug $ show args
    debug $ GHC.showPpr dflags hsmodule

    names <- getRdrNames dflags topEnv
    _opts@Options {..} <- parseArgs dflags args

    let transformNoOp :: a -> Rewrite a
        transformNoOp _ = NoRewrite

    trRebindApp <- case optRebindApp of
        Off -> return transformNoOp
        On Nothing  -> return $ transformRebindableApplication names
        On (Just rn) -> do
            let n = mkRdrName rn
            return $ transformRebindableApplication $ names { dollarName = n }

    trConstructors <- case optConstructors of
        Off -> return transformNoOp
        On Nothing -> return $ transformConstructors names
        On (Just rn) -> do
            let n = mkRdrName rn
            return $ transformConstructors $ names { buildName = n }

    let tr  = mconcat
            [ trRebindApp
            , trConstructors
            ]

    hsmodule' <- transformPs dflags tr hsmodule
    let pm' = pm { GHC.hpm_module = hsmodule' }

    return pm'

-------------------------------------------------------------------------------
-- Args parsing
-------------------------------------------------------------------------------

parseArgs :: forall m. (MonadIO m, GHC.HasLogger m) => GHC.DynFlags -> [String] -> m Options
parseArgs dflags = foldM go0 defaultOptions where
    ambWarn :: String -> String -> m ()
    ambWarn x y = warn dflags noSrcSpan $
        GHC.text ("Overloaded:" ++ x ++ " and Overloaded:" ++ y ++ " enabled")
        GHC.$$
        GHC.text ("picking Overloaded:" ++ y)

    go0 opts arg = do
        (arg', vns) <- elaborateArg arg
        go opts arg' vns

    go opts "Strings" vns = do
        when (isSym $ optStrings opts)     $ ambWarn "Symbols" "Strings"
        when (isCodeStr $ optStrings opts) $ ambWarn "CodeStrings" "Strings"

        mvn <- oneName "Strings" vns
        return $ opts { optStrings = Str mvn }

    go opts "Symbols" vns = do
        when (isStr $ optStrings opts)     $ ambWarn "Strings" "Symbols"
        when (isCodeStr $ optStrings opts) $ ambWarn "CodeStrings" "Symbols"

        mvn <- oneName "Symbols" vns
        return $ opts { optStrings = Sym mvn }

    go opts "CodeStrings" vns = do
        when (isStr $ optStrings opts) $ ambWarn "Strings" "CodeStrings"
        when (isSym $ optStrings opts) $ ambWarn "Symbols" "CodeStrings"

        mvn <- oneName "CodeStrings" vns
        return $ opts { optStrings = CodeStr mvn }

    go opts "Numerals" vns = do
        when (isNat $ optNumerals opts) $ ambWarn "Naturals" "Numerals"

        mvn <- oneName "Numerals" vns
        return $ opts { optNumerals = IsNum mvn }

    go opts "Naturals" vns = do
        when (isNum $ optNumerals opts) $ ambWarn "Numerals" "Naturals"

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
        when (isCodeLabel $ optLabels opts) $ ambWarn "CodeLabels" "Labels"

        mvn <- oneName "Labels" vns
        return $ opts { optLabels = Label mvn }
    go opts "CodeLabels" vns = do
        when (isLabel $ optLabels opts) $ ambWarn "Labels" "CodeLabels"

        mvn <- oneName "CodeLabels" vns
        return $ opts { optLabels = CodeLabel mvn }
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
    go opts "RebindableApplication" vns = do
        mrn <- oneName "RebindableApplication" vns
        return $ opts { optRebindApp = On mrn }
    go opts "Constructors" vns = do
        mrn <- oneName "Constructors" vns
        return $ opts { optConstructors = On mrn }

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
    , optLabels        :: LabelOpt
    , optUnit          :: OnOff VarName
    , optTypeNats      :: OnOff VarName
    , optTypeSymbols   :: OnOff VarName
    , optRecordFields  :: Bool
    , optIdiomBrackets :: Bool
    , optDo            :: Bool
    , optCategories    :: OnOff String -- module name
    , optRebindApp     :: OnOff VarName
    , optConstructors  :: OnOff VarName
    }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optStrings       = NoStr
    , optNumerals      = NoNum
    , optChars         = Off
    , optLists         = Off
    , optIf            = Off
    , optLabels        = NoLabel
    , optTypeNats      = Off
    , optTypeSymbols   = Off
    , optUnit          = Off
    , optRecordFields  = False
    , optIdiomBrackets = False
    , optDo            = False
    , optCategories    = Off
    , optRebindApp     = Off
    , optConstructors  = Off
    }

data StrSym
    = NoStr
    | Str (Maybe VarName)
    | Sym (Maybe VarName)
    | CodeStr (Maybe VarName)
  deriving (Eq, Show)

isSym :: StrSym -> Bool
isSym (Sym _) = True
isSym _       = False

isStr :: StrSym -> Bool
isStr (Str _) = True
isStr _       = False

isCodeStr :: StrSym -> Bool
isCodeStr (CodeStr _) = True
isCodeStr _           = False

data NumNat
    = NoNum
    | IsNum (Maybe VarName)
    | IsNat (Maybe VarName)
  deriving (Eq, Show)

isNum :: NumNat -> Bool
isNum (IsNum _) = True
isNum _         = False

isNat :: NumNat -> Bool
isNat (IsNat _) = True
isNat _         = False

data LabelOpt
    = NoLabel
    | Label (Maybe VarName)
    | CodeLabel (Maybe VarName)
  deriving (Eq, Show)

isLabel :: LabelOpt -> Bool
isLabel (Label _) = True
isLabel _         = False

isCodeLabel :: LabelOpt -> Bool
isCodeLabel (CodeLabel _) = True
isCodeLabel _             = False

data OnOff a
    = Off
    | On (Maybe a)
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- OverloadedStrings
-------------------------------------------------------------------------------

transformStrings :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformStrings Names {..} e@(L l (HsLit _ (HsString _ _fs))) =
    Rewrite $ hsApps l (hsVarA l fromStringName) [e]

transformStrings _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedSymbols
-------------------------------------------------------------------------------

transformSymbols :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformSymbols Names {..} (L l (HsLit _ (HsString _ fs))) = do
    let name' = hsVarA l fromSymbolName
    let inner = hsTyApp l name' (HsTyLit noExtField (HsStrTy GHC.NoSourceText fs))
    Rewrite inner

transformSymbols _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedCodeStrings
-------------------------------------------------------------------------------

transformCodeStrings :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformCodeStrings Names {..} e@(L l (HsLit _ (HsString _ _fs))) = do
    let inner = hsApps l (hsVarA l codeFromStringName) [e]
    WithName $ \n -> Rewrite $ L l $ HsSpliceE noAnn $ HsTypedSplice noAnn hasParens n inner

transformCodeStrings _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedNumerals
-------------------------------------------------------------------------------

transformNumerals :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformNumerals Names {..} (L l (HsOverLit _ (OverLit _ (HsIntegral (GHC.IL _ n i)) _)))
    | not n, i >= 0 = do
        let name' = hsVarA l fromNumeralName
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
    = Rewrite $ hsApps l (hsVarA l fromNaturalName) [e]

transformNaturals _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedChars
-------------------------------------------------------------------------------

transformChars :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformChars Names {..} e@(L l (HsLit _ (HsChar _ _))) =
    Rewrite $ hsApps l (hsVarA l fromCharName) [e]

transformChars _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedLists
-------------------------------------------------------------------------------

transformLists :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformLists Names {..} (L l (ExplicitList _  xs)) =
    Rewrite $ foldr cons' nil' xs
  where
    cons' :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
    cons' y ys = hsApps l (hsVarA l consName) [y, ys]

    nil' :: LHsExpr GhcRn
    nil' = hsVarA l nilName

    -- otherwise: leave intact
transformLists _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedIf
-------------------------------------------------------------------------------

transformIf :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
#if MIN_VERSION_ghc(9,0,0)
transformIf Names {..} (L l (HsIf _ co th el)) = Rewrite val4 where
#else
transformIf Names {..} (L l (HsIf _ _ co th el)) = Rewrite val4 where
#endif
    val4 = L l $ HsApp noAnn val3 el
    val3 = L l $ HsApp noAnn val2 th
    val2 = L l $ HsApp noAnn val1 co
    val1 = L l $ HsVar noExtField $ L (l2l l) ifteName
transformIf _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedLabels
-------------------------------------------------------------------------------

transformLabels :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformLabels Names {..} (L l (HsOverLabel _ fs)) = do
    let name' = hsVarA l fromLabelName
    let inner = hsTyApp l name' (HsTyLit noExtField (HsStrTy GHC.NoSourceText fs))
    Rewrite inner

transformLabels _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedCodeLabels
-------------------------------------------------------------------------------

hasParens :: SpliceDecoration
#if MIN_VERSION_ghc(9,0,0)
hasParens = DollarSplice
#else
hasParens = HasParens
#endif

transformCodeLabels :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformCodeLabels Names {..} (L l (HsOverLabel _ fs)) = do
    let name' = hsVarA l codeFromLabelName
    let inner = hsTyApp l name' (HsTyLit noExtField (HsStrTy GHC.NoSourceText fs))
    -- Rewrite $ L l $ HsRnBracketOut noExtField (ExpBr noExtField inner) []
    WithName $ \n -> Rewrite $ L l $ HsSpliceE noAnn $ HsTypedSplice noAnn hasParens n inner

transformCodeLabels _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedUnit
-------------------------------------------------------------------------------

transformUnit :: Names -> LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn)
transformUnit Names {..} (L l (HsVar _ (L _ name')))
    | name' == ghcUnitName = Rewrite (hsVarA l unitName)
  where
    ghcUnitName = GHC.getName (GHC.tupleDataCon GHC.Boxed 0)

transformUnit _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedTypeNats
-------------------------------------------------------------------------------

transformTypeNats :: Names -> LHsType GhcRn -> Rewrite (LHsType GhcRn)
transformTypeNats Names {..} e@(L l (HsTyLit _ (HsNumTy _ _))) = do
    let name' = L l $ HsTyVar noAnn NotPromoted $ L (l2l l) fromTypeNatName
    Rewrite $ L l $ HsAppTy noExtField name' e
transformTypeNats _ _ = NoRewrite

-------------------------------------------------------------------------------
-- OverloadedTypeSymbols
-------------------------------------------------------------------------------

transformTypeSymbols :: Names -> LHsType GhcRn -> Rewrite (LHsType GhcRn)
transformTypeSymbols Names {..} e@(L l (HsTyLit _ (HsStrTy _ _))) = do
    let name' = L l $ HsTyVar noAnn NotPromoted $ L (l2l l) fromTypeSymbolName
    Rewrite $ L l $ HsAppTy noExtField name' e
transformTypeSymbols _ _ = NoRewrite

-------------------------------------------------------------------------------
-- RebindableApplication
-------------------------------------------------------------------------------

transformRebindableApplication :: RdrNames -> LHsExpr GhcPs -> Rewrite (LHsExpr GhcPs)
transformRebindableApplication RdrNames {..} (L l (HsApp _ f@(L fl _) x@(L xl _)))
    = Rewrite
    $ L l $ HsPar noAnn
    $ L l $ OpApp noAnn f (L l' (HsVar noExtField (L l' dollarName))) x
  where
    l' = noAnnSrcSpan $ GHC.mkSrcSpan (GHC.srcSpanEnd $ locA fl) (GHC.srcSpanStart $ locA xl)
transformRebindableApplication _ _ = NoRewrite

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

transformConstructors :: RdrNames -> LHsExpr GhcPs -> Rewrite (LHsExpr GhcPs)
transformConstructors RdrNames {..} (L l (SectionR _ (L lop (HsVar _ (L _ op))) arg))
    | op == GHC.consDataCon_RDR
    , (L _ (HsVar _ (L _ln n)), xs) <- splitHsApps arg
    = Rewrite $ expr n xs
  where
    expr n args = hsApps_RDR l
        (hsTyApp_RDR l
            (L lop (HsVar noExtField (L (l2l lop) buildName)))
            (HsTyLit noExtField (HsStrTy GHC.NoSourceText (Plugins.occNameFS (GHC.rdrNameOcc n)))))
        [ args' ]
      where
        args' = case args of
            [x] -> x
            _   -> L l (ExplicitTuple noAnn [ Present noAnn x | x <- args ] Plugins.Boxed)

transformConstructors _ _ = NoRewrite

splitHsApps :: LHsExpr GhcPs -> (LHsExpr GhcPs, [LHsExpr GhcPs])
splitHsApps e = go e []
  where
    go :: LHsExpr GhcPs -> [LHsExpr GhcPs]
       -> (LHsExpr GhcPs, [LHsExpr GhcPs])
    go (L _ (HsApp _ f x))      xs = go f (x : xs)
    go f                        xs = (f, xs)



-------------------------------------------------------------------------------
-- Transform
-------------------------------------------------------------------------------

transformRn
    :: GHC.DynFlags
    -> (LHsExpr GhcRn -> Rewrite (LHsExpr GhcRn))
    -> HsGroup GhcRn
    -> GHC.TcM (HsGroup GhcRn)
transformRn dflags f = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcRn -> GHC.TcM (LHsExpr GhcRn)
    transform' e@(L l _) = do
        -- liftIO $ GHC.putLogMsg _dflags GHC.NoReason GHC.SevWarning _l (GHC.defaultErrStyle _dflags) $
        --     GHC.text "Expr" GHC.<+> GHC.ppr e GHC.<+> GHC.text (SYB.gshow e)
        go (f e)
      where
        go NoRewrite    = return e
        go (Rewrite e') = return e'
        go (Error err)  = do
            err dflags
            fail "Error in Overloaded plugin"
        go (WithName kont) = do
            n <- GHC.newNameAt (GHC.mkVarOcc "olSplice") $ locA l
            go (kont n)

transformPs
    :: GHC.DynFlags
    -> (LHsExpr GhcPs -> Rewrite (LHsExpr GhcPs))
#if MIN_VERSION_ghc(9,0,0)
    -> Located HsModule
    -> Plugins.Hsc (Located HsModule)
#else
    -> Located (HsModule GhcPs)
    -> Plugins.Hsc (Located (HsModule GhcPs))
#endif
transformPs dflags f = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcPs -> Plugins.Hsc (LHsExpr GhcPs)
    transform' e@(L _l _) = do
        -- liftIO $ GHC.putLogMsg _dflags GHC.NoReason GHC.SevWarning _l (GHC.defaultErrStyle _dflags) $
        --     GHC.text "Expr" GHC.<+> GHC.ppr e GHC.<+> GHC.text (SYB.gshow e)
        go (f e)
      where
        go NoRewrite    = return e
        go (Rewrite e') = return e'
        go (Error err)  = do
            err dflags
            -- Hsc doesn't have MonadFail instance
            liftIO $ throwIO $ userError "Error in Overloaded plugin"
        go (WithName _kont) = do
            liftIO $ throwIO $ userError "Error in Overloaded plugin: WithName in Ps transform"

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
            err dflags
            fail "Error in Overloaded plugin"
        go (WithName kont) = do
            n <- GHC.newNameAt (GHC.mkVarOcc "olSplice") $ locA l
            go (kont n)
