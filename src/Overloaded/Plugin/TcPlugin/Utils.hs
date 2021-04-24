module Overloaded.Plugin.TcPlugin.Utils where

import Control.Monad              (guard)
import Data.Traversable.WithIndex (itraverse)

import qualified GHC.Compat.All as GHC

import Overloaded.Plugin.V

-------------------------------------------------------------------------------
-- Simple Ct operations
-------------------------------------------------------------------------------

findClassConstraint4 :: GHC.Class -> GHC.Ct -> Maybe (GHC.Ct, V4 GHC.Type)
findClassConstraint4 cls ct = do
   (cls', [k, x, s, a]) <- GHC.getClassPredTys_maybe (GHC.ctPred ct)
   guard (cls' == cls)
   return (ct, V4 k x s a)

-- | Make newtype class evidence
makeEvidence4_1 :: GHC.Class -> GHC.CoreExpr -> V4 GHC.Type -> GHC.EvTerm
makeEvidence4_1 cls e (V4 k x s a) = GHC.EvExpr appDc where
    tyCon = GHC.classTyCon cls
    dc    = GHC.tyConSingleDataCon tyCon
    appDc = GHC.mkCoreConApps dc
        [ GHC.Type k
        , GHC.Type x
        , GHC.Type s
        , GHC.Type a
        , e
        ]

makeEvidence4_2 :: GHC.Class -> GHC.CoreExpr -> GHC.CoreExpr -> V4 GHC.Type -> GHC.EvTerm
makeEvidence4_2 cls e1 e2 (V4 k x s a) = GHC.EvExpr appDc where
    tyCon = GHC.classTyCon cls
    dc    = GHC.tyConSingleDataCon tyCon
    appDc = GHC.mkCoreConApps dc
        [ GHC.Type k
        , GHC.Type x
        , GHC.Type s
        , GHC.Type a
        , e1
        , e2
        ]



-------------------------------------------------------------------------------
-- makeVar
-------------------------------------------------------------------------------

makeVar :: String -> GHC.Type -> GHC.TcPluginM GHC.Var
makeVar n ty = do
    name <- GHC.unsafeTcPluginTcM $ GHC.newName (GHC.mkVarOcc n)
    return (GHC.mkLocalMultId name ty)

makeVars :: String -> [GHC.Type] -> GHC.TcPluginM [GHC.Var]
makeVars n tys = itraverse (\i -> makeVar (n ++ show i)) tys

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) =  a
