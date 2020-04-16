{-# LANGUAGE RecordWildCards #-}
module Overloaded.Plugin.HasField where

import Control.Monad (forM, guard, unless)
import Data.List     (elemIndex)
import Data.Maybe    (mapMaybe)

import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr
import qualified TcPluginM       as Plugins

import Overloaded.Plugin.Names
import Overloaded.Plugin.V

newtype PluginCtx = PluginCtx
    { hasPolyFieldCls :: GHC.Class
    }

tcPlugin :: GHC.TcPlugin
tcPlugin = GHC.TcPlugin
    { GHC.tcPluginInit  = tcPluginInit
    , GHC.tcPluginSolve = tcPluginSolve
    , GHC.tcPluginStop  = const (return ())
    }

tcPluginInit :: GHC.TcPluginM PluginCtx
tcPluginInit = do
    -- TODO: don't fail
    res <- Plugins.findImportedModule ghcRecordsCompatMN Nothing
    cls <- case res of
        GHC.Found _ md -> Plugins.tcLookupClass =<< Plugins.lookupOrig md (GHC.mkTcOcc "HasField")
        _              -> do
            dflags <- GHC.unsafeTcPluginTcM GHC.getDynFlags
            Plugins.tcPluginIO $ GHC.putLogMsg dflags GHC.NoReason GHC.SevError noSrcSpan (GHC.defaultErrStyle dflags) $
                GHC.text "Cannot find module" GHC.<+> GHC.ppr ghcRecordsCompatMN
            fail "panic!"

    return PluginCtx
        { hasPolyFieldCls = cls
        }

-- HasPolyField "petName" Pet Pet [Char] [Char]
tcPluginSolve :: PluginCtx -> GHC.TcPluginSolver
tcPluginSolve PluginCtx {..} _ _ wanteds = do
    -- acquire context
    dflags      <- Plugins.unsafeTcPluginTcM GHC.getDynFlags
    famInstEnvs <- Plugins.getFamInstEnvs
    rdrEnv      <- Plugins.unsafeTcPluginTcM GHC.getGlobalRdrEnv

    solved <- forM wantedsHasPolyField $ \(ct, tys@(V4 _k _name _s a)) -> do
        -- GHC.tcPluginIO $ warn dflags noSrcSpan $
        --     GHC.text "wanted" GHC.<+> GHC.ppr ct

        m <- GHC.unsafeTcPluginTcM $ matchHasField dflags famInstEnvs rdrEnv tys
        fmap (\evTerm -> (evTerm, ct)) $ forM m $ \(tc, dc, args, fl, _sel_id) -> do
            -- get location
            let ctloc = GHC.ctLoc ct
            -- let l = GHC.RealSrcSpan $ GHC.ctLocSpan ctloc

            -- debug print
            -- GHC.tcPluginIO $ warn dflags l $ GHC.text "DEBUG" GHC.$$ GHC.ppr dbg

            let s' = GHC.mkTyConApp tc args

            let (exist, theta, xs) = GHC.dataConInstSig dc args
            let fls                = GHC.dataConFieldLabels dc
            unless (length xs == length fls) $ fail "|tys| /= |fls|"

            idx <- case elemIndex fl fls of
                Nothing  -> fail "field selector not in dataCon"
                Just idx -> return idx

            -- variables we can bind to
            let exist' = exist
            let exist_ = map GHC.mkTyVarTy exist'

            theta' <- traverse (makeVar "dict") $ GHC.substTysWith exist exist_ theta
            xs'   <- traverse (makeVar "x") $ GHC.substTysWith exist exist_ xs

            let a' = xs !! idx
            let b' = a'
            let t' = s'

            bName <- GHC.unsafeTcPluginTcM $ GHC.newName (GHC.mkVarOcc "b")
            let bBndr   = GHC.mkLocalId bName $ xs !! idx

            -- (\b -> DC b x1 x2, x0)
            let rhs = GHC.mkConApp (GHC.tupleDataCon GHC.Boxed 2)
                    [ GHC.Type $ GHC.mkFunTy b' t'
                    , GHC.Type a'
                    , GHC.mkCoreLams [bBndr] $ GHC.mkConApp2 dc (args ++ exist_) $ theta' ++ replace idx bBndr xs'
                    , GHC.Var $ xs' !! idx
                    ]

            -- (a -> r, r)
            let caseType = GHC.mkTyConApp (GHC.tupleTyCon GHC.Boxed 2)
                    [ GHC.mkFunTy b' t'
                    , a'
                    ]

            -- DC x0 x1 x2 -> (\b -> DC b x1 x2, x0)
            let caseBranch = (GHC.DataAlt dc, exist' ++ theta' ++ xs', rhs)

            -- GHC.tcPluginIO $ warn dflags l $
            --     GHC.text "cases"
            --     GHC.$$
            --     GHC.ppr caseType
            --     GHC.$$
            --     GHC.ppr caseBranch


            -- \s -> case s of DC x0 x1 x2 -> (\b -> DC b x1 x2, x0)
            sName <- GHC.unsafeTcPluginTcM $ GHC.newName (GHC.mkVarOcc "s")
            let sBndr   = GHC.mkLocalId sName s'
            let expr   = GHC.mkCoreLams [sBndr] $ GHC.Case (GHC.Var sBndr) sBndr caseType [caseBranch]
            let evterm = makeEvidence4 hasPolyFieldCls expr tys

            -- wanteds
            ctEvidence <- Plugins.newWanted ctloc $ GHC.mkPrimEqPred a a'

            return (evterm, [ GHC.mkNonCanonical ctEvidence -- a ~ a'
                            ])

    return $ GHC.TcPluginOk (mapMaybe extractA solved) (concat $ mapMaybe extractB solved)
  where
    wantedsHasPolyField = mapMaybe (findClassConstraint4 hasPolyFieldCls) wanteds

    extractA (Nothing, _)     = Nothing
    extractA (Just (a, _), b) = Just (a, b)

    extractB (Nothing, _)      = Nothing
    extractB (Just (_, ct), _) = Just ct

replace :: Int -> a -> [a] -> [a]
replace _ _ []     = []
replace 0 y (_:xs) = y:xs
replace n y (x:xs) = x : replace (pred n) y xs

makeVar :: String -> GHC.Type -> GHC.TcPluginM GHC.Var
makeVar n ty = do
    name <- GHC.unsafeTcPluginTcM $ GHC.newName (GHC.mkVarOcc n)
    return (GHC.mkLocalId name ty)

-------------------------------------------------------------------------------
-- Simple Ct operations
-------------------------------------------------------------------------------

findClassConstraint4 :: GHC.Class -> GHC.Ct -> Maybe (GHC.Ct, V4 GHC.Type)
findClassConstraint4 cls ct = do
   (cls', [k, x, s, a]) <- GHC.getClassPredTys_maybe (GHC.ctPred ct)
   guard (cls' == cls)
   return (ct, V4 k x s a)

-- | Make newtype class evidence
makeEvidence4 :: GHC.Class -> GHC.CoreExpr -> V4 GHC.Type -> GHC.EvTerm
makeEvidence4 cls e (V4 k x s a) = GHC.EvExpr appDc where
    tyCon = GHC.classTyCon cls
    dc    = GHC.tyConSingleDataCon tyCon
    appDc = GHC.mkCoreConApps dc
        [ GHC.Type k
        , GHC.Type x
        , GHC.Type s
        , GHC.Type a
        , e
        ]

-------------------------------------------------------------------------------
-- Adopted from GHC
-------------------------------------------------------------------------------

matchHasField
    :: GHC.DynFlags
    -> (GHC.FamInstEnv, GHC.FamInstEnv)
    -> GHC.GlobalRdrEnv
    -> V4 GHC.Type
    -> GHC.TcM (Maybe (GHC.TyCon, GHC.DataCon, [GHC.Type], GHC.FieldLabel, GHC.Id))
matchHasField _dflags famInstEnvs rdrEnv (V4 _k x s _a)
    -- x should be a literal string
    | Just xStr <- GHC.isStrLitTy x
    -- s should be an applied type constructor
    , Just (tc, args) <- GHC.tcSplitTyConApp_maybe s
    -- use representation tycon (if data family); it has the fields
    , let s_tc = fstOf3 (GHC.tcLookupDataFamInst famInstEnvs tc args)
    -- x should be a field of r
    , Just fl <- GHC.lookupTyConFieldLabel xStr s_tc
    -- the field selector should be in scope
    , Just _gre <- GHC.lookupGRE_FieldLabel rdrEnv fl
    -- and the type should have only single data constructor (for simplicity)
    , Just [dc] <- GHC.tyConDataCons_maybe tc
    = do
        sel_id <- GHC.tcLookupId (GHC.flSelector fl)
        (_tv_prs, _preds, sel_ty) <- GHC.tcInstType GHC.newMetaTyVars sel_id

        -- The selector must not be "naughty" (i.e. the field
        -- cannot have an existentially quantified type), and
        -- it must not be higher-rank.
        if not (GHC.isNaughtyRecordSelector sel_id) && GHC.isTauTy sel_ty
        then return $ Just (tc, dc, args, fl, sel_id)
        else return Nothing

matchHasField _ _ _ _ = return Nothing

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) =  a
