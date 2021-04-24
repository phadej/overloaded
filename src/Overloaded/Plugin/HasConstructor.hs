{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Overloaded.Plugin.HasConstructor where

import Control.Monad (forM)
import Data.List     (find)
import Data.Maybe    (mapMaybe)

import qualified GHC.Compat.All  as GHC

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Tc.Plugin as Plugins
#else
import qualified TcPluginM as Plugins
#endif

import Overloaded.Plugin.Diagnostics
import Overloaded.Plugin.TcPlugin.Ctx
import Overloaded.Plugin.TcPlugin.Utils
import Overloaded.Plugin.V

ifDebug :: Monad m => m () -> m ()
ifDebug _ = return ()

solveHasConstructor
    :: PluginCtx
    -> GHC.DynFlags
    -> (GHC.FamInstEnv, GHC.FamInstEnv)
    -> GHC.GlobalRdrEnv
    -> [GHC.Ct]
    -> Plugins.TcPluginM [(Maybe (GHC.EvTerm, [GHC.Ct]), GHC.Ct)]
solveHasConstructor PluginCtx {..} dflags famInstEnvs rdrEnv wanteds =
    forM wantedsHasPolyCon $ \(ct, tys@(V4 _k _name _s a)) -> do
        -- Plugins.tcPluginIO $ warn dflags noSrcSpan $
        --     GHC.text "HasConstructor wanted" GHC.<+> GHC.ppr ct

        m <- GHC.unsafeTcPluginTcM $ matchHasConstructor dflags famInstEnvs rdrEnv tys
        fmap (\evTerm -> (evTerm, ct)) $ forM m $ \(tc, dc, args, xs) -> do
            -- get location
            let ctloc = GHC.ctLoc ct
            let l = GHC.RealSrcSpan (GHC.ctLocSpan ctloc)
#if MIN_VERSION_ghc(9,0,0)
                        Nothing
#endif

            ifDebug $ Plugins.tcPluginIO $ warn dflags l $
                GHC.text "DEBUG1"
                    GHC.$$ GHC.ppr tc
                    GHC.$$ GHC.ppr dc
                    GHC.$$ GHC.ppr args
                    GHC.$$ GHC.ppr xs

            -- "s"
            let s' = GHC.mkTyConApp tc args

            -- type of constructor fields:
            -- - for unary constructors we use the field
            -- - for nullary we use unit
            -- - for others we wrap them in tuple.
            let a' :: GHC.Type
                a' = case xs of
                    [x] -> x
                    _   -> GHC.mkBoxedTupleTy xs
                    -- TODO: nullary
                    -- TODO: multiple

            -- let b' = a'
            --     t' = s'

            let tupleDataCon :: GHC.DataCon
                tupleDataCon = GHC.tupleDataCon GHC.Boxed (length xs)

            ifDebug $ Plugins.tcPluginIO $ warn dflags l $
                GHC.text "DEBUG2"
                    GHC.$$ GHC.ppr s'
                    GHC.$$ GHC.ppr a'

            -- build
            exprBuild <- case xs of
                -- unary: \a -> DC a
                [_] -> do
                    x <- makeVar "x" a'
                    return $ GHC.mkCoreLams [x] $ GHC.mkConApp2 dc args [x]

                -- nullary: \ (_unused :: ()) -> DC
                [] -> do
                    unused <- makeVar "_unused" a'
                    return $ GHC.mkCoreLams [unused] $ GHC.mkConApp2 dc args []

                -- multi: \ (a :: a) -> case a of
                --    (x1, ..., xn) -> DC x1 ... xn
                _ -> do
                    aBndr <- makeVar "a" a'
                    xs' <- makeVars "x" xs
                    return $ GHC.mkCoreLams [aBndr] $ GHC.Case (GHC.Var aBndr) aBndr s'
                        [( GHC.DataAlt tupleDataCon  -- (,,,)
                        , xs'                        -- x1 ... xn
                        , GHC.mkConApp2 dc args xs'  -- DC x1 ... xn
                        )]

            ifDebug $ Plugins.tcPluginIO $ warn dflags l $
                GHC.text "DEBUG-build"
                    GHC.$$ GHC.ppr exprBuild

            -- match
            exprMatch <- case xs of
                -- unary: \s -> case s of
                --            DC a -> Just a
                --            _    -> Nothing
                [_] -> do
                    sBndr <- makeVar "s" s'
                    aBndr <- makeVar "a" a'
                    return $ GHC.mkCoreLams [sBndr] $ GHC.Case (GHC.Var sBndr) sBndr
                        (GHC.mkTyConApp GHC.maybeTyCon [a'])
                        -- default case have to be first.
                        [ (GHC.DEFAULT, [], GHC.mkConApp2 GHC.nothingDataCon [a'] [])
                        , (GHC.DataAlt dc, [aBndr], GHC.mkConApp2 GHC.justDataCon [a'] [aBndr])
                        ]

                -- nullary: \s -> case s of
                --              DC -> Just ()
                --              _  -> Nothing
                [] -> do
                    sBndr <- makeVar "s" s'
                    return $ GHC.mkCoreLams [sBndr] $ GHC.Case (GHC.Var sBndr) sBndr
                        (GHC.mkTyConApp GHC.maybeTyCon [a'])
                        [ (GHC.DEFAULT, [], GHC.mkConApp2 GHC.nothingDataCon [a'] [])
                        , (GHC.DataAlt dc, [], GHC.mkConApp2 GHC.justDataCon [a'] [GHC.unitDataConId])
                        ]

                -- multi: \s -> case s of
                --            DC x1 ... xn -> let a = (x1, ... xn) in Just a
                --            _            -> Nothing
                _ -> do
                    sBndr <- makeVar "s" s'
                    aBndr <- makeVar "a" a'
                    xs' <- makeVars "x" xs

                    return $ GHC.mkCoreLams [sBndr] $ GHC.Case (GHC.Var sBndr) sBndr
                        (GHC.mkTyConApp GHC.maybeTyCon [a'])
                        [ (GHC.DEFAULT, [], GHC.mkConApp2 GHC.nothingDataCon [a'] [])
                        , (GHC.DataAlt dc, xs',
                          GHC.Let (GHC.NonRec aBndr $ GHC.mkConApp2 tupleDataCon xs xs') $
                          GHC.mkConApp2 GHC.justDataCon [a'] [aBndr])
                        ]

            ifDebug $ Plugins.tcPluginIO $ warn dflags l $
                GHC.text "DEBUG-match"
                    GHC.$$ GHC.ppr exprMatch

            -- wanteds
            let evterm = makeEvidence4_2 hasPolyConCls exprBuild exprMatch tys
            ctEvidence <- Plugins.newWanted ctloc $ GHC.mkPrimEqPred a a'

            return (evterm, [ GHC.mkNonCanonical ctEvidence -- a ~ a'
                            ])

  where
    wantedsHasPolyCon = mapMaybe (findClassConstraint4 hasPolyConCls) wanteds

matchHasConstructor
    :: GHC.DynFlags
    -> (GHC.FamInstEnv, GHC.FamInstEnv)
    -> GHC.GlobalRdrEnv
    -> V4 GHC.Type
    -> GHC.TcM (Maybe (GHC.TyCon, GHC.DataCon, [GHC.Type], [GHC.Type]))
matchHasConstructor _dflags famInstEnvs _rdrEnv (V4 _k x s _a)
    -- x should be a literal string
    | Just xStr <- GHC.isStrLitTy x
    -- s should be an applied type constructor
    , Just (tc, args) <- GHC.tcSplitTyConApp_maybe s
    -- use representation tycon (if data family); it has the fields
    , let s_tc = fstOf3 (GHC.tcLookupDataFamInst famInstEnvs tc args)
    -- x should be constructor of r
    , Just dcs <- GHC.tyConDataCons_maybe s_tc
    , Just dc  <- find (\dc -> GHC.getOccFS (GHC.dataConName dc) == xStr) dcs
    -- TODO: check that data con is in scope

    -- check that exist and theta are empty, this makes things simpler!
    , ([], [], xs) <- GHC.dataConInstSig dc args

    = return $ Just (tc, dc, args, xs)

matchHasConstructor _ _ _ _ = return Nothing


