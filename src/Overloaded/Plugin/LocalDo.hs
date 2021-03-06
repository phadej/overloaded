{-# LANGUAGE CPP #-}
module Overloaded.Plugin.LocalDo where

import qualified Data.Generics   as SYB
import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Plugins     as Plugins
#else
import qualified GhcPlugins      as Plugins
#endif

import Overloaded.Plugin.Diagnostics
import Overloaded.Plugin.Names
import Overloaded.Plugin.Rewrite

#if MIN_VERSION_ghc(9,0,0)
#define _bufspan _
#else
#define _bufspan
#endif

transformDo
    :: Names
    -> LHsExpr GhcRn
    -> Rewrite (LHsExpr GhcRn)
transformDo names (L l (OpApp _ (L (RealSrcSpan l1 _bufspan) (HsVar _ (L _ doName)))
                                (L (RealSrcSpan l2 _bufspan) (HsVar _ (L _ compName')))
                                (L (RealSrcSpan l3 _bufspan)
#if MIN_VERSION_ghc(9,0,0)
                                   (HsDo _ (DoExpr Nothing) (L _ stmts))
#else
                                   (HsDo _ DoExpr (L _ stmts))
#endif
                                   )))
    | spanNextTo l1 l2
    , spanNextTo l2 l3
    , compName' == composeName names
    = case transformDo' names doName l stmts of
        Right x  -> Rewrite x
        Left err -> Error err
transformDo _ _ = NoRewrite

transformDo' :: Names -> GHC.Name -> SrcSpan -> [ExprLStmt GhcRn] -> Either (GHC.DynFlags -> IO ()) (LHsExpr GhcRn)
transformDo' _names _doName l [] = Left $ \dflags ->
    putError dflags l $ GHC.text "Empty do"
#if MIN_VERSION_ghc(9,0,0)
transformDo'  names  doName _ (L l (BindStmt _ pat body) : next) = do
#else
transformDo'  names  doName _ (L l (BindStmt _ pat body _ _) : next) = do
#endif
    next' <- transformDo' names doName l next
    return $ hsApps l bind [ body, kont next' ]
  where
    bind  = hsTyApp l (hsVar l doName) (hsTyVar l (doBindName names))
    kont next' = L l $ HsLam noExtField MG
        { mg_ext    = noExtField
        , mg_alts   = L l $ pure $ L l Match
            { m_ext   = noExtField
            , m_ctxt  = LambdaExpr
            , m_pats  = [pat]
            , m_grhss = GRHSs
                { grhssExt        = noExtField
                , grhssGRHSs      = [ L noSrcSpan $ GRHS noExtField [] $ next' ]
                , grhssLocalBinds = L noSrcSpan $ EmptyLocalBinds noExtField
                }
            }
        , mg_origin = Plugins.Generated
        }
transformDo'  names  doName _ (L l (BodyStmt _ body _ _) : next) = do
    next' <- transformDo' names doName l next
    return $ hsApps l then_ [ body, next' ]
  where
    then_ = hsTyApp l (hsVar l doName) (hsTyVar l (doThenName names))

transformDo' _ _ _ [L _ (LastStmt _ body _ _)] = return body
transformDo' _ _ _ (L l stmt : _) = Left $ \dflags ->
    putError dflags l $ GHC.text "Unsupported statement in do"
        GHC.$$ GHC.ppr stmt
        GHC.$$ GHC.text (SYB.gshow stmt)

spanNextTo :: RealSrcSpan -> RealSrcSpan -> Bool
spanNextTo x y
    = srcSpanStartLine y == srcSpanEndLine x
    && srcSpanStartCol y == srcSpanEndCol x
