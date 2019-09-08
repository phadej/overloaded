{-# LANGUAGE RecordWildCards #-}
module Overloaded (plugin) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Generics          as SYB

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
pluginImpl args env gr = do
    dflags <- GHC.getDynFlags
    debug $ GHC.showPpr dflags gr
    names <- getNames
    when (null args) $
        warn dflags noSrcSpan $ GHC.text "No overloaded features enabled"
    gr1 <- case () of
        _ | "Symbols" `elem` args -> transformSymbols dflags names env gr
        _ | "Strings" `elem` args -> transformStrings dflags names env gr
        _ -> return gr

    gr2 <- case () of
        _ | "Numerals" `elem` args -> transformNumerals dflags names env gr1
        _ -> return gr1

    return (env, gr2)

-------------------------------------------------------------------------------
-- OverloadedStrings
-------------------------------------------------------------------------------

transformStrings
    :: GHC.DynFlags
    -> Names
    -> TcRnTypes.TcGblEnv
    -> HsGroup GhcRn
    -> TcRnTypes.TcM (HsGroup GhcRn)
transformStrings _dflags Names {..} _env = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcRn -> TcRnTypes.TcM (LHsExpr GhcRn)
    transform' e@(L l (HsLit _ (HsString _ _fs))) = do
        return $ L l $ HsApp noExt (L l (HsVar noExt (L l fromStringName))) e

    -- otherwise: leave intact
    transform' expr =
        return expr

-------------------------------------------------------------------------------
-- OverloadedSymbols
-------------------------------------------------------------------------------

transformSymbols
    :: GHC.DynFlags
    -> Names
    -> TcRnTypes.TcGblEnv
    -> HsGroup GhcRn
    -> TcRnTypes.TcM (HsGroup GhcRn)
transformSymbols _dflags Names {..} _env = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcRn -> TcRnTypes.TcM (LHsExpr GhcRn)
    transform' (L l (HsLit _ (HsString _ fs))) = do
        let name' = (L l (HsVar noExt (L l fromSymbolName)))
        let inner = L l $ HsAppType (HsWC [] (L l (HsTyLit noExt (HsStrTy GHC.NoSourceText fs)))) name'
        return inner

    -- otherwise: leave intact
    transform' expr =
        return expr

-------------------------------------------------------------------------------
-- OverloadedNumerals
-------------------------------------------------------------------------------

transformNumerals
    :: GHC.DynFlags
    -> Names
    -> TcRnTypes.TcGblEnv
    -> HsGroup GhcRn
    -> TcRnTypes.TcM (HsGroup GhcRn)
transformNumerals _dflags Names {..} _env = SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcRn -> TcRnTypes.TcM (LHsExpr GhcRn)
    transform' (L l (HsOverLit _ (OverLit _ (HsIntegral (GHC.IL _ n i)) _))) | n == False, i >= 0 = do
        let name' = (L l (HsVar noExt (L l fromNumeralName)))
        let inner = L l $ HsAppType (HsWC [] (L l (HsTyLit noExt (HsNumTy GHC.NoSourceText i)))) name'
        return inner

    -- otherwise: leave intact
    transform' expr =
        return expr

-------------------------------------------------------------------------------
-- ModuleNames
-------------------------------------------------------------------------------

dataStringMN :: GHC.ModuleName
dataStringMN =  GHC.mkModuleName "Data.String"

overloadedSymbolsMN :: GHC.ModuleName
overloadedSymbolsMN =  GHC.mkModuleName "Overloaded.Symbols"

overloadedNumeralsMN :: GHC.ModuleName
overloadedNumeralsMN =  GHC.mkModuleName "Overloaded.Numerals"

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

data Names = Names
    { fromStringName  :: GHC.Name
    , fromSymbolName  :: GHC.Name
    , fromNumeralName :: GHC.Name
    }

getNames :: TcRnTypes.TcM Names
getNames = do
    env <- TcM.getTopEnv

    GHC.Found _ md <- liftIO $ Finder.findImportedModule env dataStringMN Nothing
    name <- IfaceEnv.lookupOrig md (GHC.mkVarOcc "fromString")

    GHC.Found _ md' <- liftIO $ Finder.findImportedModule env overloadedSymbolsMN Nothing
    name' <- IfaceEnv.lookupOrig md' (GHC.mkVarOcc "fromSymbol")

    GHC.Found _ md'' <- liftIO $ Finder.findImportedModule env overloadedNumeralsMN Nothing
    name'' <- IfaceEnv.lookupOrig md'' (GHC.mkVarOcc "fromNatural")

    return Names
        { fromStringName = name
        , fromSymbolName = name'
        , fromNumeralName = name''
        }

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
