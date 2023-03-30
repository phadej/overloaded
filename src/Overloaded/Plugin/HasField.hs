{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Overloaded.Plugin.HasField where

import Control.Monad (forM, unless)
import Data.List     (elemIndex)
import Data.Maybe    (mapMaybe)

import qualified GHC.Compat.All  as GHC

import qualified GHC.Tc.Plugin as Plugins

import Overloaded.Plugin.V
import Overloaded.Plugin.TcPlugin.Ctx
import Overloaded.Plugin.TcPlugin.Utils

-- HasPolyField "petName" Pet Pet [Char] [Char]
solveHasField
    :: PluginCtx
    -> GHC.DynFlags
    -> (GHC.FamInstEnv, GHC.FamInstEnv)
    -> GHC.GlobalRdrEnv
    -> [GHC.Ct]
    -> Plugins.TcPluginM [(Maybe (GHC.EvTerm, [GHC.Ct]), GHC.Ct)]
solveHasField PluginCtx {..} dflags famInstEnvs rdrEnv wanteds =
    forM wantedsHasPolyField $ \(ct, tys@(V4 _k _name _s a)) -> do
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

            bBndr <- makeVar "b" (xs !! idx)

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
            let caseBranch = GHC.Alt (GHC.DataAlt dc) (exist' ++ theta' ++ xs') rhs

            -- GHC.tcPluginIO $ warn dflags l $
            --     GHC.text "cases"
            --     GHC.$$
            --     GHC.ppr caseType
            --     GHC.$$
            --     GHC.ppr caseBranch


            -- \s -> case s of DC x0 x1 x2 -> (\b -> DC b x1 x2, x0)
            sName <- GHC.unsafeTcPluginTcM $ GHC.newName (GHC.mkVarOcc "s")
            let sBndr  = GHC.mkLocalMultId sName s'
            let expr   = GHC.mkCoreLams [sBndr] $ GHC.Case (GHC.Var sBndr) sBndr caseType [caseBranch]
            let evterm = makeEvidence4_1 hasPolyFieldCls expr tys

            -- wanteds
            ctEvidence <- Plugins.newWanted ctloc $ GHC.mkPrimEqPred a a'

            return (evterm, [ GHC.mkNonCanonical ctEvidence -- a ~ a'
                            ])
  where
    wantedsHasPolyField = mapMaybe (findClassConstraint4 hasPolyFieldCls) wanteds

replace :: Int -> a -> [a] -> [a]
replace _ _ []     = []
replace 0 y (_:xs) = y:xs
replace n y (x:xs) = x : replace (pred n) y xs

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
