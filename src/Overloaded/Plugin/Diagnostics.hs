{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Overloaded.Plugin.Diagnostics where

import Control.Monad.IO.Class (MonadIO (..))

import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr
import qualified GHC.Tc.Plugin as Plugins

-------------------------------------------------------------------------------
-- Doesn't really belong here
-------------------------------------------------------------------------------

-- TODO maybe use HasDynFlags instead of dynFlags param
putMsg :: GHC.Logger -> GHC.DynFlags -> GHC.SDoc -> IO ()
#if MIN_VERSION_ghc(9,4,0)
putMsg logger _dynFlags sDoc = GHC.putMsg logger sDoc
#else
putMsg = GHC.putMsg
#endif

putPluginDebugMsg :: (MonadIO m, GHC.HasLogger m) => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
putPluginDebugMsg dflags l doc = do
    let mkMsg =
#if MIN_VERSION_ghc(9,4,0)
            GHC.mkLocMessage GHC.MCDump
#else
            GHC.mkLocMessage GHC.SevWarning
#endif
    logger <- GHC.getLogger
    liftIO $ putMsg logger dflags $ mkMsg l doc

tcPluginDebugMsg :: GHC.DynFlags -> SrcSpan -> GHC.SDoc -> Plugins.TcPluginM ()
tcPluginDebugMsg dflags l doc = do
    let mkMsg =
#if MIN_VERSION_ghc(9,4,0)
            GHC.mkLocMessage GHC.MCDump
#else
            GHC.mkLocMessage GHC.SevWarning
#endif
    hscEnv <- Plugins.getTopEnv
    let logger = GHC.hsc_logger hscEnv
    Plugins.tcPluginIO $ putMsg logger dflags $ mkMsg l doc

putPluginUsageWarn :: (MonadIO m, GHC.HasLogger m) => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
putPluginUsageWarn dflags l doc = do
#if MIN_VERSION_ghc(9,4,0)
    let diagOpts = GHC.pDiagOpts $ GHC.initParserOpts dflags
        mkMsg = GHC.mkLocMessage $ GHC.mkMCDiagnostic diagOpts GHC.WarningWithoutFlag
#else
    let mkMsg = GHC.mkLocMessage GHC.SevWarning
#endif
    logger <- GHC.getLogger
    liftIO $ putMsg logger dflags $ mkMsg l doc

putPluginUsageErr :: (MonadIO m, GHC.HasLogger m) => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
putPluginUsageErr dflags l doc = do
#if MIN_VERSION_ghc(9,4,0)
    let diagOpts = GHC.pDiagOpts $ GHC.initParserOpts dflags
        mkMsg = GHC.mkLocMessage $ GHC.mkMCDiagnostic diagOpts GHC.ErrorWithoutFlag
#else
    let mkMsg = GHC.mkLocMessage GHC.SevError
#endif
    logger <- GHC.getLogger
    liftIO $ putMsg logger dflags $ mkMsg l doc

putInternalPluginErr :: (MonadIO m, GHC.HasLogger m) => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
putInternalPluginErr dflags l doc = do
    let mkMsg =
#if MIN_VERSION_ghc(9,4,0)
            GHC.mkLocMessage GHC.MCFatal
#else
            GHC.mkLocMessage GHC.SevError
#endif
    logger <- GHC.getLogger
    liftIO $ putMsg logger dflags $ mkMsg l doc

-- TcPluginM doesn't implement HasLogger
tcInternalPluginErr :: GHC.DynFlags -> SrcSpan -> GHC.SDoc -> Plugins.TcPluginM ()
tcInternalPluginErr dflags l doc = do
    let mkMsg =
#if MIN_VERSION_ghc(9,4,0)
            GHC.mkLocMessage GHC.MCFatal
#else
            GHC.mkLocMessage GHC.SevError
#endif
    hscEnv <- Plugins.getTopEnv
    let logger = GHC.hsc_logger hscEnv
    Plugins.tcPluginIO $ putMsg logger dflags $ mkMsg l doc

-- | for avoiding impredicativity when needed
newtype GhcDiagMonadWrapper a = GhcDiagMonadWrapper (forall m. (MonadIO m, GHC.HasLogger m) => m a)

debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn
debug _ = pure ()
