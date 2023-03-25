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

#if MIN_VERSION_ghc(9,0,0)
#define ERR_STYLE
#else
#define ERR_STYLE (GHC.defaultErrStyle dflags)
#endif

#if MIN_VERSION_ghc(9,2,0)

putError :: (MonadIO m, GHC.HasLogger m) => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
putError dflags l doc = do
    let doc' = GHC.mkLocMessage GHC.SevError l doc
    logger <- GHC.getLogger
    liftIO $ GHC.putMsg logger dflags doc'

warn :: (MonadIO m, GHC.HasLogger m) => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
warn dflags l doc = do
    let doc' = GHC.mkLocMessage GHC.SevWarning l doc
    logger <- GHC.getLogger
    liftIO $ GHC.putMsg logger dflags doc'

-- TcPluginM doesn't implement HasLogger
tcWarn :: GHC.DynFlags -> SrcSpan -> GHC.SDoc -> Plugins.TcPluginM ()
tcWarn dflags l doc = do
    hscEnv <- Plugins.getTopEnv
    let logger = GHC.hsc_logger hscEnv
    let doc' = GHC.mkLocMessage GHC.SevWarning l doc
    Plugins.tcPluginIO $ GHC.putMsg logger dflags doc'

-- TcPluginM doesn't implement HasLogger
tcError :: GHC.DynFlags -> SrcSpan -> GHC.SDoc -> Plugins.TcPluginM ()
tcError dflags l doc = do
    hscEnv <- Plugins.getTopEnv
    let logger = GHC.hsc_logger hscEnv
    let doc' = GHC.mkLocMessage GHC.SevError l doc
    Plugins.tcPluginIO $ GHC.putMsg logger dflags doc'

-- | for avoiding impredicativity when needed
newtype GhcDiagMonadWrapper a = GhcDiagMonadWrapper (forall m. (MonadIO m, GHC.HasLogger m) => m a)

#else

putError :: MonadIO m => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
putError dflags l doc =
    liftIO $ GHC.putLogMsg dflags GHC.NoReason GHC.SevError l ERR_STYLE doc

warn :: MonadIO m => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
warn dflags l doc =
    liftIO $ GHC.putLogMsg dflags GHC.NoReason GHC.SevWarning l ERR_STYLE doc

-- | for avoiding impredicativity when needed
newtype GhcDiagMonadWrapper a = GhcDiagMonadWrapper (forall m. (MonadIO m) => m a)

#endif

debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn
debug _ = pure ()
