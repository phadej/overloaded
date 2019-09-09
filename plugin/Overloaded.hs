{-# LANGUAGE RecordWildCards #-}
module Overloaded (plugin) where

import Control.Applicative    ((<|>))
import Control.Monad          (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List              (foldl')
import Data.List.Split        (splitOn)

import qualified Data.Generics as SYB

-- GHC stuff
import qualified ErrUtils   as Err
import qualified Finder
import qualified GhcPlugins as GHC
import           HsSyn
import qualified IfaceEnv
import           SrcLoc
import qualified TcRnMonad  as TcM
import qualified TcRnTypes

-------------------------------------------------------------------------------
-- Plugin
-------------------------------------------------------------------------------

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
    { GHC.renamedResultAction = pluginImpl
    , GHC.pluginRecompile     = GHC.purePlugin
    }

pluginImpl
    :: [GHC.CommandLineOption]
    -> TcRnTypes.TcGblEnv
    -> HsGroup GhcRn
    -> TcRnTypes.TcM (TcRnTypes.TcGblEnv, HsGroup GhcRn)
pluginImpl args' env gr = do
    dflags <- GHC.getDynFlags
    debug $ show args
    debug $ GHC.showPpr dflags gr
    names <- getNames
    opts@Options {..} <- parseArgs dflags args
    when (opts == defaultOptions) $
        warn dflags noSrcSpan $ GHC.text "No Overloaded features enabled"

{-
    gr1 <- case () of
        _ | "Symbols" `elem` args -> transformSymbols dflags names env gr
        _ | "Strings" `elem` args -> transformStrings dflags names env gr
        _ -> return gr

    gr2 <- case () of
        _ | "Numerals" `elem` args -> transformNumerals dflags names env gr1
        _ -> return gr1

    gr3 <- case () of
        _ | "Lists" `elem` args -> transformLists dflags names env gr2
        _ -> return gr2
-}

    let transformNoOp :: LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
        transformNoOp _ = Nothing

        trStr = case optStrings of
            Just Str -> transformStrings names
            Just Sym -> transformSymbols names
            Nothing  -> transformNoOp

        trNum | optNumerals = transformNumerals names
              | otherwise   = transformNoOp

        trLists | optLists  = transformLists names
                | otherwise = transformNoOp

        trIf | optIf     = transformIf names
             | otherwise = transformNoOp

        tr = trStr /\ trNum /\ trLists /\ trIf

    gr' <- transform dflags tr gr

    return (env, gr')
  where
    args = concatMap (splitOn ":") args'

    (/\) :: (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
    f /\ g = \x -> f x <|> g x

    infixr 9 /\

-------------------------------------------------------------------------------
-- Args parasing
-------------------------------------------------------------------------------

parseArgs :: GHC.DynFlags -> [String] -> TcRnTypes.TcM Options
parseArgs dflags = foldM go defaultOptions where
    go opts "Strings" = do
        when (optStrings opts == Just Sym) $ warn dflags noSrcSpan $
            GHC.text "Overloaded:Strings and Overloaded:Symbols enabled"
            GHC.$$
            GHC.text "picking Overloaded:Strings"

        return $ opts { optStrings = Just Str }

    go opts "Symbols" = do
        when (optStrings opts == Just Sym) $ warn dflags noSrcSpan $
            GHC.text "Overloaded:Strings and Overloaded:Symbols enabled"
            GHC.$$
            GHC.text "picking Overloaded:Symbols"

        return $ opts { optStrings = Just Sym }

    go opts "Numerals" = return $ opts { optNumerals = True }
    go opts "Lists"    = return $ opts { optLists = True }
    go opts "If"       = return $ opts { optIf = True }

    go opts s = do
        warn dflags noSrcSpan $ GHC.text $ "Unknown Overloaded option " ++  show s
        return opts

data Options = Options
    { optStrings  :: Maybe Str
    , optNumerals :: Bool
    , optLists    :: Bool
    , optIf       :: Bool
    }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optStrings  = Nothing
    , optNumerals = False
    , optLists    = False
    , optIf       = False
    }

data Str = Str | Sym deriving (Eq, Show)

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
    let inner = L l $ HsAppType (HsWC [] (L l (HsTyLit noExt (HsStrTy GHC.NoSourceText fs)))) name'
    Just inner

transformSymbols _ _ = Nothing

-------------------------------------------------------------------------------
-- OverloadedNumerals
-------------------------------------------------------------------------------

transformNumerals :: Names -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
transformNumerals Names {..} (L l (HsOverLit _ (OverLit _ (HsIntegral (GHC.IL _ n i)) _)))
    | not n, i >= 0 = do
        let name' = hsVar l fromNumeralName
        let inner = L l $ HsAppType (HsWC [] (L l (HsTyLit noExt (HsNumTy GHC.NoSourceText i)))) name'
        Just inner

transformNumerals _ _ = Nothing

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

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

hsVar :: SrcSpan -> GHC.Name -> LHsExpr GhcRn
hsVar l n = L l (HsVar noExt (L l n))

hsApps :: SrcSpan -> LHsExpr GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
hsApps l = foldl' app where
    app :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
    app f x = L l (HsApp noExt f x)

-------------------------------------------------------------------------------
-- ModuleNames
-------------------------------------------------------------------------------

dataStringMN :: GHC.ModuleName
dataStringMN =  GHC.mkModuleName "Data.String"

overloadedSymbolsMN :: GHC.ModuleName
overloadedSymbolsMN =  GHC.mkModuleName "Overloaded.Symbols"

overloadedNumeralsMN :: GHC.ModuleName
overloadedNumeralsMN =  GHC.mkModuleName "Overloaded.Numerals"

overloadedListsMN :: GHC.ModuleName
overloadedListsMN =  GHC.mkModuleName "Overloaded.Lists"

overloadedIfMN :: GHC.ModuleName
overloadedIfMN =  GHC.mkModuleName "Overloaded.If"

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

data Names = Names
    { fromStringName  :: GHC.Name
    , fromSymbolName  :: GHC.Name
    , fromNumeralName :: GHC.Name
    , nilName         :: GHC.Name
    , consName        :: GHC.Name
    , ifteName        :: GHC.Name
    }

getNames :: TcRnTypes.TcM Names
getNames = do
    env <- TcM.getTopEnv

    fromStringName  <- lookupVar env dataStringMN "fromString"
    fromSymbolName  <- lookupVar env overloadedSymbolsMN "fromSymbol"
    fromNumeralName <- lookupVar env overloadedNumeralsMN "fromNatural"
    nilName         <- lookupVar env overloadedListsMN "nil"
    consName        <- lookupVar env overloadedListsMN "cons"
    ifteName        <- lookupVar env overloadedIfMN "ifte"

    return Names {..}
  where
    lookupVar env mn vn = do
        res <-  liftIO $ Finder.findImportedModule env mn Nothing
        case res of
            GHC.Found _ md -> IfaceEnv.lookupOrig md (GHC.mkVarOcc vn)
            _              -> fail "panic!"


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
