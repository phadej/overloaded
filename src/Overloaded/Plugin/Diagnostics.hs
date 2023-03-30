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

debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn
debug _ = pure ()
