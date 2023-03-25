{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP             #-}
module Overloaded.Plugin.TcPlugin.Ctx where

import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Tc.Plugin as Plugins
#else
import qualified TcPluginM as Plugins
#endif

import Overloaded.Plugin.Diagnostics
import Overloaded.Plugin.Names

data PluginCtx = PluginCtx
    { hasPolyFieldCls :: GHC.Class
    , hasPolyConCls   :: GHC.Class
    }

tcPluginInit :: GHC.TcPluginM PluginCtx
tcPluginInit = do
    dflags <- GHC.unsafeTcPluginTcM GHC.getDynFlags

    let findModule :: GHC.ModuleName -> Plugins.TcPluginM GHC.Module
        findModule m = do
            im <- Plugins.findImportedModule m Nothing
            case im of
                GHC.Found _ md -> return md
                _              -> do
                    tcError dflags noSrcSpan  $
                        GHC.text "Cannot find module" GHC.<+> GHC.ppr m
                    fail "panic!"

    hasPolyFieldCls <- do
        md <- findModule ghcRecordsCompatMN
        Plugins.tcLookupClass =<< Plugins.lookupOrig md (GHC.mkTcOcc "HasField")

    hasPolyConCls <- do
        md <- findModule overloadedConstructorsMN
        Plugins.tcLookupClass =<< Plugins.lookupOrig md (GHC.mkTcOcc "HasConstructor")

    return PluginCtx {..}
