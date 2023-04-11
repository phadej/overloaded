{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP             #-}
module Overloaded.Plugin.TcPlugin.Ctx where

import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr

import qualified GHC.Tc.Plugin as Plugins

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
            im <- Plugins.findImportedModule m
#if MIN_VERSION_ghc(9,4,0)
                GHC.NoPkgQual
#else
                Nothing
#endif
            case im of
                GHC.Found _ md -> return md
                _              -> do
                    tcInternalPluginErr dflags noSrcSpan  $
                        GHC.text "Cannot find module" GHC.<+> GHC.ppr m
                    fail "panic!"

    hasPolyFieldCls <- do
        md <- findModule ghcRecordsCompatMN
        Plugins.tcLookupClass =<< Plugins.lookupOrig md (GHC.mkTcOcc "HasField")

    hasPolyConCls <- do
        md <- findModule overloadedConstructorsMN
        Plugins.tcLookupClass =<< Plugins.lookupOrig md (GHC.mkTcOcc "HasConstructor")

    return PluginCtx {..}
