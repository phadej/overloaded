{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Overloaded.Plugin.TcPlugin (
    tcPlugin,
) where

import Data.Maybe    (mapMaybe)

import qualified GHC.Compat.All  as GHC

import qualified GHC.Tc.Plugin as Plugins

import Overloaded.Plugin.TcPlugin.Ctx
import Overloaded.Plugin.HasField
import Overloaded.Plugin.HasConstructor

-- TODO: take argument which options to enable.
tcPlugin :: GHC.TcPlugin
tcPlugin = GHC.TcPlugin
    { GHC.tcPluginInit  = tcPluginInit
    , GHC.tcPluginSolve = tcPluginSolve
    , GHC.tcPluginStop  = const (pure ())
#if MIN_VERSION_ghc(9,4,0)
    , GHC.tcPluginRewrite = const GHC.emptyUFM
#endif
    }

-- HasPolyField "petName" Pet Pet [Char] [Char]
tcPluginSolve :: PluginCtx -> GHC.TcPluginSolver
tcPluginSolve ctx _ _ wanteds = do
    -- acquire context
    dflags      <- Plugins.unsafeTcPluginTcM GHC.getDynFlags
    famInstEnvs <- Plugins.getFamInstEnvs
    rdrEnv      <- Plugins.unsafeTcPluginTcM GHC.getGlobalRdrEnv

    solvedHasField       <- solveHasField       ctx dflags famInstEnvs rdrEnv wanteds
    solvedHasConstructor <- solveHasConstructor ctx dflags famInstEnvs rdrEnv wanteds

    let solved = solvedHasField ++ solvedHasConstructor

    return $ GHC.TcPluginOk (mapMaybe extractA solved) (concat $ mapMaybe extractB solved)
  where
    extractA (Nothing, _)     = Nothing
    extractA (Just (a, _), b) = Just (a, b)

    extractB (Nothing, _)      = Nothing
    extractB (Just (_, ct), _) = Just ct
