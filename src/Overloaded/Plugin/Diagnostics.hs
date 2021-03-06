{-# LANGUAGE CPP #-}
module Overloaded.Plugin.Diagnostics where

import Control.Monad.IO.Class (MonadIO (..))

import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr

-------------------------------------------------------------------------------
-- Doesn't really belong here
-------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,0,0)
#define ERR_STYLE
#else
#define ERR_STYLE (GHC.defaultErrStyle dflags)
#endif

putError :: MonadIO m => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
putError dflags l doc =
    liftIO $ GHC.putLogMsg dflags GHC.NoReason GHC.SevError l ERR_STYLE doc

warn :: MonadIO m => GHC.DynFlags -> SrcSpan -> GHC.SDoc -> m ()
warn dflags l doc =
    liftIO $ GHC.putLogMsg dflags GHC.NoReason GHC.SevWarning l ERR_STYLE doc

debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn
debug _ = pure ()
