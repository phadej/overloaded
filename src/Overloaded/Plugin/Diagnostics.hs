module Overloaded.Plugin.Diagnostics where

import Control.Monad.IO.Class (MonadIO (..))

import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr

-------------------------------------------------------------------------------
-- Doesn't really belong here
-------------------------------------------------------------------------------

putError :: MonadIO m => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
putError dflags l doc =
    liftIO $ GHC.putLogMsg dflags GHC.NoReason GHC.SevError l (GHC.defaultErrStyle dflags) doc

warn :: MonadIO m => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
warn dflags l doc =
    liftIO $ GHC.putLogMsg dflags GHC.NoReason GHC.SevWarning l (GHC.defaultErrStyle dflags) doc

debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn
debug _ = pure ()
