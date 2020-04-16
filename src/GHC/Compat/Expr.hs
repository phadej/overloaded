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
    hsTyApp, 
    hsTyVar,
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
    -- * Reader phase
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
) where

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs
#else
import HsSyn
#endif

#if MIN_VERSION_ghc(8,8,0)
import BasicTypes (PromotionFlag (..))
#endif

import Data.List (foldl')
import SrcLoc
       (GenLocated (..), Located, RealSrcSpan, SrcSpan (..), noSrcSpan,
       srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)

import qualified Name as GHC

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

hsTyApp :: SrcSpan -> LHsExpr GhcRn -> HsType GhcRn -> LHsExpr GhcRn
#if MIN_VERSION_ghc(8,8,0)
hsTyApp l x ty = L l $ HsAppType noExtField x (HsWC [] (L l ty))
#else
hsTyApp l x ty = L l $ HsAppType (HsWC [] (L l ty)) x
#endif
