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
    hsApps,
    hsApps_RDR,
    matchHsApps,
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
#if MIN_VERSION_ghc(8,8,0)
    PromotionFlag (..),
#else
    Promoted (..),
#endif
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
    RealSrcSpan,
    noSrcSpan,
    srcSpanStartLine,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanEndCol,
    -- * Extensions
    noExtField,
    -- * Names
    nameToString,
) where

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs
#else
import HsSyn
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic (PromotionFlag (..))
#elif MIN_VERSION_ghc(8,8,0)
import BasicTypes (PromotionFlag (..))
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc
       (GenLocated (..), Located, RealSrcSpan, SrcSpan (..), noSrcSpan,
       srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
#else
import SrcLoc
       (GenLocated (..), Located, RealSrcSpan, SrcSpan (..), noSrcSpan,
       srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
#endif

import Data.List (foldl')

import qualified GHC.Compat.All as GHC

#if !(MIN_VERSION_ghc(8,10,0))
noExtField  :: NoExt
noExtField = noExt
#endif

hsVar :: SrcSpan -> GHC.Name -> LHsExpr GhcRn
hsVar l n = L l (HsVar noExtField (L l n))

hsTyVar :: SrcSpan -> GHC.Name -> HsType GhcRn
hsTyVar l n = HsTyVar noExtField NotPromoted (L l n)

hsApps :: SrcSpan -> LHsExpr GhcRn -> [LHsExpr GhcRn] -> LHsExpr GhcRn
hsApps l = foldl' app where
    app :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
    app f x = L l (HsApp noExtField f x)

matchHsApps :: LHsExpr GhcRn -> (LHsExpr GhcRn, [LHsExpr GhcRn])
matchHsApps (L _ (HsApp _ f x)) =
    let (g, ys) = matchHsApps f
    in (g, ys ++ [x])
matchHsApps e = (e, [])

hsApps_RDR :: SrcSpan -> LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
hsApps_RDR l = foldl' app where
    app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
    app f x = L l (HsApp noExtField f x)

hsOpApp :: SrcSpan -> LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
hsOpApp l x op y = L l (OpApp GHC.defaultFixity x op y)

hsTyApp :: SrcSpan -> LHsExpr GhcRn -> HsType GhcRn -> LHsExpr GhcRn
#if MIN_VERSION_ghc(8,8,0)
hsTyApp l x ty = L l $ HsAppType noExtField x (HsWC [] (L l ty))
#else
hsTyApp l x ty = L l $ HsAppType (HsWC [] (L l ty)) x
#endif

hsTyApp_RDR :: SrcSpan -> LHsExpr GhcPs -> HsType GhcPs -> LHsExpr GhcPs
#if MIN_VERSION_ghc(8,8,0)
hsTyApp_RDR l x ty = L l $ HsAppType noExtField x (HsWC noExtField (L l ty))
#else
hsTyApp_RDR l x ty = L l $ HsAppType (HsWC noExtField (L l ty)) x
#endif

-- | Construct simple lambda @\(pat) -> body@.
hsLam :: SrcSpan -> LPat GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
hsLam l pat body = L l $ HsLam noExtField MG
    { mg_ext    = noExtField
    , mg_alts   = L l $ pure $ L l Match
        { m_ext   = noExtField
        , m_ctxt  = LambdaExpr
        , m_pats  = [pat]
        , m_grhss = GRHSs
            { grhssExt        = noExtField
            , grhssGRHSs      = [ L noSrcSpan $ GRHS noExtField [] body ]
            , grhssLocalBinds = L noSrcSpan $ EmptyLocalBinds noExtField
            }
        }
    , mg_origin = GHC.Generated
    }

hsPar :: SrcSpan -> LHsExpr GhcRn -> LHsExpr GhcRn
hsPar l e = L l (HsPar noExtField e)

nameToString :: GHC.Name -> String
nameToString = GHC.occNameString . GHC.occName
