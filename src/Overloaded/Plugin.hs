{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Overloaded plugin, which makes magic possible.
module Overloaded.Plugin (plugin) where

import Control.Applicative    ((<|>))
import Control.Monad          (foldM, forM, guard, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List              (elemIndex, foldl', intercalate)
import Data.List.Split        (splitOn)
import Data.Maybe             (mapMaybe)
import Data.Maybe             (catMaybes)

import qualified Data.Generics as SYB

-- GHC stuff
import qualified Class
import qualified ErrUtils   as Err
import qualified FamInst
import qualified FamInstEnv
import qualified Finder
import qualified GhcPlugins as GHC
import           HsSyn      as GHC
import qualified IfaceEnv
import qualified RdrName
import           SrcLoc
import qualified TcEnv
import qualified TcEvidence as Tc
import qualified TcMType
import qualified TcPluginM  as TC
import qualified TcRnMonad  as TcM
import qualified TcRnTypes

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
-- * @Chars@ desugars literal characters to @'Overloaded.Chars.fromChars' c@. /Note:/ there isn't type-level alternative: we cannot promote 'Char's.
-- * @Lists@ __is not__ like built-in @OverloadedLists@, but desugars explicit lists to 'Overloaded.Lists.cons' and 'Overloaded.Lists.nil'
-- * @If@ desugars @if@-expressions to @'Overloaded.If.ifte' b t e@
-- * @Labels@ works like built-in @OverloadedLabels@ (you should enable @OverloadedLabels@ so parser recognises the syntax)
-- * @TypeNats@ and @TypeSymbols@ desugar type-level literals into @'Overloaded.TypeNats.FromNat'@ and @'Overloaded.TypeSymbols.FromTypeSymbol'@ respectively.
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
plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.renamedResultAction = pluginImpl
    , GHC.tcPlugin            = enabled tcPlugin
    , GHC.pluginRecompile     = GHC.purePlugin
    }
  where
    enabled p args'
        | "RecordFields" `elem` args = Just p
        | otherwise                  = Nothing
      where
        args = concatMap (splitOn ":") args'

pluginImpl
    :: [GHC.CommandLineOption]
    -> TcRnTypes.TcGblEnv
    -> HsGroup GhcRn
    -> TcRnTypes.TcM (TcRnTypes.TcGblEnv, HsGroup GhcRn)
pluginImpl args' env gr = do
    dflags <- GHC.getDynFlags
    topEnv <- TcM.getTopEnv

    debug $ show args
    debug $ GHC.showPpr dflags gr

    names <- getNames dflags topEnv
    opts@Options {..} <- parseArgs dflags args
    when (opts == defaultOptions) $
        warn dflags noSrcSpan $ GHC.text "No Overloaded features enabled"

    let transformNoOp :: a -> Maybe a
        transformNoOp _ = Nothing

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

    let tr  = trStr /\ trNum /\ trChr /\ trLists /\ trIf /\ trLabel
    let trT = trTypeNats /\ trTypeSymbols

    gr' <- transformType dflags trT gr
    gr'' <- transform dflags tr gr'

    return (env, gr'')
  where
    args = concatMap (splitOn ":") args'

    (/\) :: (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
    f /\ g = \x -> f x <|> g x

    infixr 9 /\ -- hello CPP

-------------------------------------------------------------------------------
-- Args parsing
-------------------------------------------------------------------------------

parseArgs :: forall m. MonadIO m => GHC.DynFlags -> [String] -> m Options
parseArgs dflags = foldM go0 defaultOptions where
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
    go opts "Labels"   vns = do
        mvn <- oneName "Symbols" vns
        return $ opts { optLabels = On mvn }
    go opts "TypeNats" vns = do
        mvn <- oneName "TypeNats" vns
        return $ opts { optTypeNats = On mvn }
    go opts "TypeSymbols" vns = do
        mvn <- oneName "TypeSymbols" vns
        return $ opts { optTypeSymbols = On mvn }
    go opts "RecordFields" _ =
        return $ opts { optRecordFields = True }

    go opts s _ = do
        warn dflags noSrcSpan $ GHC.text $ "Unknown Overloaded option " ++  show s
        return opts

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
    { optStrings      :: StrSym
    , optNumerals     :: NumNat
    , optChars        :: OnOff VarName
    , optLists        :: OnOff (V2 VarName)
    , optIf           :: OnOff VarName
    , optLabels       :: OnOff VarName
    , optTypeNats     :: OnOff VarName
    , optTypeSymbols  :: OnOff VarName
    , optRecordFields :: Bool
    }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optStrings      = NoStr
    , optNumerals     = NoNum
    , optChars        = Off
    , optLists        = Off
    , optIf           = Off
    , optLabels       = Off
    , optTypeNats     = Off
    , optTypeSymbols  = Off
    , optRecordFields = False
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

-------------------------------------------------------------------------------
-- OverloadedStrings
-------------------------------------------------------------------------------

transformStrings :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformStrings Names {..} e@(L l (HsLit _ (HsString _ _fs))) =
    Just $ hsApps l (hsVar l fromStringName) [e]

transformStrings _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedSymbols
-------------------------------------------------------------------------------

transformSymbols :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformSymbols Names {..} (L l (HsLit _ (HsString _ fs))) = do
    let name' = hsVar l fromSymbolName
    let inner = hsTyApp l name' (HsTyLit noExt (HsStrTy GHC.NoSourceText fs))
    Just inner

transformSymbols _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedNumerals
-------------------------------------------------------------------------------

transformNumerals :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformNumerals Names {..} (L l (HsOverLit _ (OverLit _ (HsIntegral (GHC.IL _ n i)) _)))
    | not n, i >= 0 = do
        let name' = hsVar l fromNumeralName
        let inner = hsTyApp l name' (HsTyLit noExt (HsNumTy GHC.NoSourceText i))
        Just inner

transformNumerals _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedNaturals
-------------------------------------------------------------------------------

transformNaturals :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformNaturals Names {..} e@(L l (HsOverLit _ (OverLit _ (HsIntegral (GHC.IL _ n i)) _)))
    | not n, i >= 0 = do
    Just $ hsApps l (hsVar l fromNaturalName) [e]

transformNaturals _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedChars
-------------------------------------------------------------------------------

transformChars :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformChars Names {..} e@(L l (HsLit _ (HsChar _ _))) =
    Just $ hsApps l (hsVar l fromCharName) [e]

transformChars _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedLists
-------------------------------------------------------------------------------

transformLists :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformLists Names {..} (L l (ExplicitList _ Nothing xs)) =
    Just $ foldr cons' nil' xs
  where
    cons' :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
    cons' y ys = hsApps l (hsVar l consName) [y, ys]

    nil' :: LHsExpr GhcRn
    nil' = hsVar l nilName

    -- otherwise: leave intact
transformLists _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedIf
-------------------------------------------------------------------------------

transformIf :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformIf Names {..} (L l (HsIf _ _ co th el)) = Just val4 where
    val4 = L l $ HsApp noExt val3 el
    val3 = L l $ HsApp noExt val2 th
    val2 = L l $ HsApp noExt val1 co
    val1 = L l $ HsVar noExt $ L l ifteName
transformIf _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedLabels
-------------------------------------------------------------------------------

transformLabels :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformLabels Names {..} (L l (HsOverLabel _ Nothing fs)) = do
    let name' = hsVar l fromLabelName
    let inner = hsTyApp l name' (HsTyLit noExt (HsStrTy GHC.NoSourceText fs))
    Just inner

transformLabels _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedTypeNats
-------------------------------------------------------------------------------

transformTypeNats :: Names -> LHsType GhcRn -> Maybe  (LHsType GhcRn)
transformTypeNats Names {..} e@(L l (HsTyLit _ (HsNumTy _ _))) = do
    let name' = L l $ HsTyVar noExt GHC.NotPromoted $ L l fromTypeNatName
    Just $ L l $ HsAppTy noExt name' e
transformTypeNats _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedTypeSymbols
-------------------------------------------------------------------------------

transformTypeSymbols :: Names -> LHsType GhcRn -> Maybe  (LHsType GhcRn)
transformTypeSymbols Names {..} e@(L l (HsTyLit _ (HsStrTy _ _))) = do
    let name' = L l $ HsTyVar noExt GHC.NotPromoted $ L l fromTypeSymbolName
    Just $ L l $ HsAppTy noExt name' e
transformTypeSymbols _ _ = Nothing

-------------------------------------------------------------------------------
-- Transform
-------------------------------------------------------------------------------

transform
    :: GHC.DynFlags
    -> (LHsExpr GhcRn -> Maybe (LHsExpr GhcRn))
    -> HsGroup GhcRn
    -> TcRnTypes.TcM (HsGroup GhcRn)
transform _dflags f = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcRn -> TcRnTypes.TcM (LHsExpr GhcRn)
    transform' e = do
        return $ case f e of
            Just e' -> e'
            Nothing -> e

transformType
    :: GHC.DynFlags
    -> (LHsType GhcRn -> Maybe (LHsType GhcRn))
    -> HsGroup GhcRn
    -> TcRnTypes.TcM (HsGroup GhcRn)
transformType _dflags f = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsType GhcRn -> TcRnTypes.TcM (LHsType GhcRn)
    transform' e = do
        return $ case f e of
            Just e' -> e'
            Nothing -> e

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

hsVar :: SrcSpan -> GHC.Name -> LHsExpr GhcRn
hsVar l n = L l (HsVar noExt (L l n))

hsApps :: SrcSpan -> LHsExpr GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
hsApps l = foldl' app where
    app :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
    app f x = L l (HsApp noExt f x)

hsTyApp :: SrcSpan -> LHsExpr GhcRn -> HsType GhcRn -> LHsExpr GhcRn
#if MIN_VERSION_ghc(8,8,0)
hsTyApp l x ty = L l $ HsAppType noExt x (HsWC [] (L l ty))
#else
hsTyApp l x ty = L l $ HsAppType (HsWC [] (L l ty)) x
#endif

-------------------------------------------------------------------------------
-- ModuleNames
-------------------------------------------------------------------------------

dataStringMN :: GHC.ModuleName
dataStringMN =  GHC.mkModuleName "Data.String"

overloadedCharsMN :: GHC.ModuleName
overloadedCharsMN =  GHC.mkModuleName "Overloaded.Chars"

overloadedSymbolsMN :: GHC.ModuleName
overloadedSymbolsMN =  GHC.mkModuleName "Overloaded.Symbols"

overloadedNaturalsMN :: GHC.ModuleName
overloadedNaturalsMN =  GHC.mkModuleName "Overloaded.Naturals"

overloadedNumeralsMN :: GHC.ModuleName
overloadedNumeralsMN =  GHC.mkModuleName "Overloaded.Numerals"

overloadedListsMN :: GHC.ModuleName
overloadedListsMN =  GHC.mkModuleName "Overloaded.Lists"

overloadedIfMN :: GHC.ModuleName
overloadedIfMN =  GHC.mkModuleName "Overloaded.If"

ghcOverloadedLabelsMN :: GHC.ModuleName
ghcOverloadedLabelsMN =  GHC.mkModuleName "GHC.OverloadedLabels"

overloadedTypeNatsMN :: GHC.ModuleName
overloadedTypeNatsMN =  GHC.mkModuleName "Overloaded.TypeNats"

overloadedTypeSymbolsMN :: GHC.ModuleName
overloadedTypeSymbolsMN =  GHC.mkModuleName "Overloaded.TypeSymbols"

overloadedRecordFieldsMN :: GHC.ModuleName
overloadedRecordFieldsMN =  GHC.mkModuleName "GHC.Records.Compat"

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

data Names = Names
    { fromStringName     :: GHC.Name
    , fromSymbolName     :: GHC.Name
    , fromNumeralName    :: GHC.Name
    , fromNaturalName    :: GHC.Name
    , fromCharName       :: GHC.Name
    , nilName            :: GHC.Name
    , consName           :: GHC.Name
    , ifteName           :: GHC.Name
    , fromLabelName      :: GHC.Name
    , fromTypeNatName    :: GHC.Name
    , fromTypeSymbolName :: GHC.Name
    }

getNames :: GHC.DynFlags -> GHC.HscEnv -> TcRnTypes.TcM Names
getNames dflags env = do
    fromStringName  <- lookupName dflags env dataStringMN "fromString"
    fromSymbolName  <- lookupName dflags env overloadedSymbolsMN "fromSymbol"
    fromNumeralName <- lookupName dflags env overloadedNumeralsMN "fromNumeral"
    fromNaturalName <- lookupName dflags env overloadedNaturalsMN "fromNatural"
    fromCharName    <- lookupName dflags env overloadedCharsMN "fromChar"
    nilName         <- lookupName dflags env overloadedListsMN "nil"
    consName        <- lookupName dflags env overloadedListsMN "cons"
    ifteName        <- lookupName dflags env overloadedIfMN "ifte"
    fromLabelName   <- lookupName dflags env ghcOverloadedLabelsMN "fromLabel"

    fromTypeNatName    <- lookupName' dflags env overloadedTypeNatsMN "FromNat"
    fromTypeSymbolName <- lookupName' dflags env overloadedTypeSymbolsMN "FromTypeSymbol"

    return Names {..}

lookupName :: GHC.DynFlags -> GHC.HscEnv -> GHC.ModuleName -> String -> TcM.TcM GHC.Name
lookupName dflags env mn vn = do
    res <-  liftIO $ Finder.findImportedModule env mn Nothing
    case res of
        GHC.Found _ md -> IfaceEnv.lookupOrig md (GHC.mkVarOcc vn)
        _              -> do
            liftIO $ GHC.putLogMsg dflags GHC.NoReason Err.SevError noSrcSpan (GHC.defaultErrStyle dflags) $
                GHC.text "Cannot find module" GHC.<+> GHC.ppr mn
            fail "panic!"

lookupName' :: GHC.DynFlags -> GHC.HscEnv -> GHC.ModuleName -> String -> TcM.TcM GHC.Name
lookupName' dflags env mn vn = do
    res <-  liftIO $ Finder.findImportedModule env mn Nothing
    case res of
        GHC.Found _ md -> IfaceEnv.lookupOrig md (GHC.mkTcOcc vn)
        _              -> do
            liftIO $ GHC.putLogMsg dflags GHC.NoReason Err.SevError noSrcSpan (GHC.defaultErrStyle dflags) $
                GHC.text "Cannot find module" GHC.<+> GHC.ppr mn
            fail "panic!"

-- | Module name and variable name
data VarName = VN String String
  deriving (Eq, Show)

lookupVarName :: GHC.DynFlags -> GHC.HscEnv -> VarName -> TcM.TcM GHC.Name
lookupVarName dflags env (VN vn mn) = lookupName dflags env (GHC.mkModuleName vn) mn

lookupTypeName :: GHC.DynFlags -> GHC.HscEnv -> VarName -> TcM.TcM GHC.Name
lookupTypeName dflags env (VN vn mn) = lookupName' dflags env (GHC.mkModuleName vn) mn

-------------------------------------------------------------------------------
-- diagnostics
-------------------------------------------------------------------------------

warn :: MonadIO m => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
warn dflags l doc =
    liftIO $ GHC.putLogMsg dflags GHC.NoReason Err.SevWarning l (GHC.defaultErrStyle dflags) doc
        --     GHC.text "parsed string"
        --     GHC.$$
        --     GHC.ppr fs

debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn
debug _ = pure ()

-------------------------------------------------------------------------------
-- V2 and V4
-------------------------------------------------------------------------------

data V2 a = V2 a a
  deriving (Eq, Show)

data V4 a = V4 a a a a
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Type-checker plugin
-------------------------------------------------------------------------------

newtype PluginCtx = PluginCtx
    { hasPolyFieldCls :: Class.Class
    }

tcPlugin :: TcM.TcPlugin
tcPlugin = TcM.TcPlugin
    { TcM.tcPluginInit  = tcPluginInit
    , TcM.tcPluginSolve = tcPluginSolve
    , TcM.tcPluginStop  = const (return ())
    }

tcPluginInit :: TC.TcPluginM PluginCtx
tcPluginInit = do
    -- TODO: don't fail
    res <- TC.findImportedModule overloadedRecordFieldsMN Nothing
    cls <- case res of
        GHC.Found _ md -> TC.tcLookupClass =<< TC.lookupOrig md (GHC.mkTcOcc "HasField")
        _              -> do
            dflags <- TC.unsafeTcPluginTcM GHC.getDynFlags
            TC.tcPluginIO $ GHC.putLogMsg dflags GHC.NoReason Err.SevError noSrcSpan (GHC.defaultErrStyle dflags) $
                GHC.text "Cannot find module" GHC.<+> GHC.ppr overloadedRecordFieldsMN
            fail "panic!"

    return PluginCtx
        { hasPolyFieldCls = cls
        }

-- HasPolyField "petName" Pet Pet [Char] [Char]
tcPluginSolve :: PluginCtx -> TcRnTypes.TcPluginSolver
tcPluginSolve PluginCtx {..} _ _ wanteds = do
    -- acquire context
    dflags      <- TC.unsafeTcPluginTcM GHC.getDynFlags
    famInstEnvs <- TC.getFamInstEnvs
    rdrEnv      <- TC.unsafeTcPluginTcM TcM.getGlobalRdrEnv

    solved <- forM wantedsHasPolyField $ \(ct, tys@(V4 _k _name _s a)) -> do
        -- TC.tcPluginIO $ warn dflags noSrcSpan $
        --     GHC.text "wanted" GHC.<+> GHC.ppr ct

        m <- TC.unsafeTcPluginTcM $ matchHasField dflags famInstEnvs rdrEnv tys
        fmap (\evTerm -> (evTerm, ct)) $ forM m $ \(tc, dc, args, fl, _sel_id) -> do
            -- get location
            let ctloc = TcM.ctLoc ct
            -- let l = GHC.RealSrcSpan $ TcM.ctLocSpan ctloc

            -- debug print
            -- TC.tcPluginIO $ warn dflags l $ GHC.text "DEBUG" GHC.$$ GHC.ppr dbg

            let s' = GHC.mkTyConApp tc args

            let (exist, theta, xs) = GHC.dataConInstSig dc args
            let fls                = GHC.dataConFieldLabels dc
            unless (length xs == length fls) $ fail "|tys| /= |fls|"

            idx <- case elemIndex fl fls of
                Nothing  -> fail "field selector not in dataCon"
                Just idx -> return idx

            -- variables we can bind to
            let exist' = exist
            let exist_ = map GHC.mkTyVarTy exist'

            theta' <- traverse (makeVar "dict") $ GHC.substTysWith exist exist_ theta
            xs'   <- traverse (makeVar "x") $ GHC.substTysWith exist exist_ xs

            let a' = xs !! idx
            let b' = a'
            let t' = s'

            bName <- TC.unsafeTcPluginTcM $ TcM.newName (GHC.mkVarOcc "b")
            let bBndr   = GHC.mkLocalId bName $ xs !! idx

            -- (\b -> DC b x1 x2, x0)
            let rhs = GHC.mkConApp (GHC.tupleDataCon GHC.Boxed 2)
                    [ GHC.Type $ GHC.mkFunTy b' t'
                    , GHC.Type a'
                    , GHC.mkCoreLams [bBndr] $ GHC.mkConApp2 dc (args ++ exist_) $ theta' ++ replace idx bBndr xs'
                    , GHC.Var $ xs' !! idx
                    ]

            -- (a -> r, r)
            let caseType = GHC.mkTyConApp (GHC.tupleTyCon GHC.Boxed 2)
                    [ GHC.mkFunTy b' t'
                    , a'
                    ]

            -- DC x0 x1 x2 -> (\b -> DC b x1 x2, x0)
            let caseBranch = (GHC.DataAlt dc, exist' ++ theta' ++ xs', rhs)

            -- TC.tcPluginIO $ warn dflags l $
            --     GHC.text "cases"
            --     GHC.$$
            --     GHC.ppr caseType
            --     GHC.$$
            --     GHC.ppr caseBranch


            -- \s -> case s of DC x0 x1 x2 -> (\b -> DC b x1 x2, x0)
            sName <- TC.unsafeTcPluginTcM $ TcM.newName (GHC.mkVarOcc "s")
            let sBndr   = GHC.mkLocalId sName s'
            let expr   = GHC.mkCoreLams [sBndr] $ GHC.Case (GHC.Var sBndr) sBndr caseType [caseBranch]
            let evterm = makeEvidence4 hasPolyFieldCls expr tys

            -- wanteds
            ctEvidence <- TC.newWanted ctloc $ GHC.mkPrimEqPred a a'

            return (evterm, [ TcM.mkNonCanonical ctEvidence -- a ~ a'
                            ])

    return $ TcRnTypes.TcPluginOk (mapMaybe extractA solved) (concat $ mapMaybe extractB solved)
  where
    wantedsHasPolyField = mapMaybe (findClassConstraint4 hasPolyFieldCls) wanteds

    extractA (Nothing, _)     = Nothing
    extractA (Just (a, _), b) = Just (a, b)

    extractB (Nothing, _)      = Nothing
    extractB (Just (_, ct), _) = Just ct

replace :: Int -> a -> [a] -> [a]
replace _ _ []     = []
replace 0 y (_:xs) = y:xs
replace n y (x:xs) = x : replace (pred n) y xs

makeVar :: String -> GHC.Type -> TcRnTypes.TcPluginM GHC.Var
makeVar n ty = do
    name <- TC.unsafeTcPluginTcM $ TcM.newName (GHC.mkVarOcc n)
    return (GHC.mkLocalId name ty)

-------------------------------------------------------------------------------
-- Simple Ct operations
-------------------------------------------------------------------------------

findClassConstraint4 :: Class.Class -> TcM.Ct -> Maybe (TcM.Ct, V4 GHC.Type)
findClassConstraint4 cls ct = do
   (cls', [k, x, s, a]) <- GHC.getClassPredTys_maybe (TcM.ctPred ct)
   guard (cls' == cls)
   return (ct, V4 k x s a)

-- | Make newtype class evidence
makeEvidence4 :: Class.Class -> GHC.CoreExpr -> V4 GHC.Type -> Tc.EvTerm
makeEvidence4 cls e (V4 k x s a) = Tc.EvExpr appDc where
    tyCon = Class.classTyCon cls
    dc    = GHC.tyConSingleDataCon tyCon
    appDc = GHC.mkCoreConApps dc
        [ GHC.Type k
        , GHC.Type x
        , GHC.Type s
        , GHC.Type a
        , e
        ]
-------------------------------------------------------------------------------
-- Adopted from GHC
-------------------------------------------------------------------------------


matchHasField
    :: GHC.DynFlags
    -> (FamInstEnv.FamInstEnv, FamInstEnv.FamInstEnv)
    -> RdrName.GlobalRdrEnv
    -> V4 GHC.Type
    -> TcM.TcM (Maybe (GHC.TyCon, GHC.DataCon, [GHC.Type], GHC.FieldLabel, GHC.Id))
matchHasField _dflags famInstEnvs rdrEnv (V4 _k x s _a)
    -- x should be a literal string
    | Just xStr <- GHC.isStrLitTy x
    -- s should be an applied type constructor
    , Just (tc, args) <- GHC.tcSplitTyConApp_maybe s
    -- use representation tycon (if data family); it has the fields
    , let s_tc = fstOf3 (FamInst.tcLookupDataFamInst famInstEnvs tc args)
    -- x should be a field of r
    , Just fl <- GHC.lookupTyConFieldLabel xStr s_tc
    -- the field selector should be in scope
    , Just _gre <- RdrName.lookupGRE_FieldLabel rdrEnv fl
    -- and the type should have only single data constructor (for simplicity)
    , Just [dc] <- GHC.tyConDataCons_maybe tc
    = do
        sel_id <- TcEnv.tcLookupId (GHC.flSelector fl)
        (_tv_prs, _preds, sel_ty) <- TcMType.tcInstType TcMType.newMetaTyVars sel_id

        -- The selector must not be "naughty" (i.e. the field
        -- cannot have an existentially quantified type), and
        -- it must not be higher-rank.
        if not (GHC.isNaughtyRecordSelector sel_id) && GHC.isTauTy sel_ty
        then return $ Just (tc, dc, args, fl, sel_id)
        else return Nothing

matchHasField _ _ _ _ = return Nothing

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) =  a

