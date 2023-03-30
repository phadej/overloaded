{-# LANGUAGE CPP #-}
-- | THis module re-exports 'HsExpr' and few related data types.
module GHC.Compat.Expr (
    -- * Expression
    HsExpr (..),
    LHsExpr,
    HsBracket (..),
    HsStmtContext (..),
    StmtLR (..),
    ExprLStmt,
    MatchGroup (..),
    Match (..),
    GRHSs (..),
    GRHS (..),
    HsMatchContext (..),
    HsLocalBindsLR (..),
    -- ** Constructors
    hsVar,
    hsVarA,
    hsApps,
    hsApps_RDR,
    hsTyApp,
    hsTyApp_RDR,
    hsTyVar,
    hsLam,
    hsPar,
    hsOpApp,
    -- * Accessors
    hsConPatArgs,
    -- * Patterns
    LPat,
    Pat (..),
    -- * Splices
    HsSplice (..),
    SpliceDecoration (..),
    -- * Proc commands
    HsCmdTop (..),
    HsCmd (..),
    LHsCmd,
    CmdLStmt,
    HsArrAppType (..),
    -- * Tuples
    HsTupArg (..),
    -- * Literals
    HsLit (..),
    HsTyLit (..),
    HsOverLit (..),
    OverLitVal (..),
    -- * Type
    HsType (..),
    LHsType,
    HsWildCardBndrs (..),
    PromotionFlag (..),
    -- * Statements
    HsGroup,
    HsModule,
    -- * phases
    GhcPs,
    GhcRn,
    -- * SourceSpan
    Located,
    GenLocated (..),
    SrcSpan (..),
    SrcSpanAnnA,
    SrcSpanAnnN,
    RealSrcSpan,
    l2l,
    locA,
    noAnn,
    noAnnSrcSpan,
    noSrcSpan,
    noSrcSpanA,
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
    -- * Extensions
    noExtField,
    -- * Names
    nameToString,
) where

import GHC.Hs
import GHC.Types.Basic (PromotionFlag (..))

import GHC.Types.SrcLoc
       (GenLocated (..), Located, RealSrcSpan, SrcSpan (..), noSrcSpan,
       srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)

import Data.List (foldl')

import qualified GHC.Compat.All as GHC

-- | preserves NamenAnn
hsVar :: SrcSpanAnnN -> GHC.Name -> LHsExpr GhcRn
hsVar l n = L (l2l l) (HsVar noExtField (L l n))

-- | preserves AnnListItem
hsVarA :: SrcSpanAnnA -> GHC.Name -> LHsExpr GhcRn
hsVarA l n = L l (HsVar noExtField (L (l2l l) n))

hsTyVar :: SrcSpanAnnN -> GHC.Name -> HsType GhcRn
hsTyVar l n = HsTyVar noAnn NotPromoted (L l n)

hsApps :: SrcSpanAnnA -> LHsExpr GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
hsApps l = foldl' app where
    app :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
    app f x = L l (HsApp noAnn f x)



hsApps_RDR :: SrcSpanAnnA -> LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
hsApps_RDR l = foldl' app where
    app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
    app f x = L l (HsApp noAnn f x)

hsOpApp :: SrcSpanAnnA -> LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
hsOpApp l x op y = L l (OpApp GHC.defaultFixity x op y)

hsTyApp :: SrcSpanAnnA -> LHsExpr GhcRn -> HsType GhcRn -> LHsExpr GhcRn
hsTyApp l x ty = L l $ HsAppType noExtField x (HsWC [] (L l ty))

hsTyApp_RDR :: SrcSpanAnnA -> LHsExpr GhcPs -> HsType GhcPs -> LHsExpr GhcPs
hsTyApp_RDR l x ty = L l $ HsAppType noSrcSpan x (HsWC noExtField (L l ty))

-- | Construct simple lambda @\(pat) -> body@.
hsLam :: SrcSpanAnnA -> LPat GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
hsLam l pat body = L l $ HsLam noExtField MG
    { mg_ext    = noExtField
    , mg_alts   = L (l2l l) $ pure $ L l Match
        { m_ext   = noAnn
        , m_ctxt  = LambdaExpr
        , m_pats  = [pat]
        , m_grhss = GRHSs
            { grhssExt        = emptyComments
            , grhssGRHSs      = [ L noSrcSpan $ GRHS noAnn [] body ]
            , grhssLocalBinds = EmptyLocalBinds noExtField
            }
        }
    , mg_origin = GHC.Generated
    }

hsPar :: SrcSpanAnnA -> LHsExpr GhcRn -> LHsExpr GhcRn
hsPar l e = L l (HsPar noAnn e)

nameToString :: GHC.Name -> String
nameToString = GHC.occNameString . GHC.occName


