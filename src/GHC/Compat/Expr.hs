{-# LANGUAGE CPP #-}
-- | THis module re-exports 'HsExpr' and few related data types.
module GHC.Compat.Expr (
    -- * Expression
    HsExpr (..),
    LHsExpr,
    HsBracket (..),
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
    SrcSpan,
    noSrcSpan,
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

import SrcLoc (Located, SrcSpan, GenLocated (..), noSrcSpan)

#if !(MIN_VERSION_ghc(8,10,0))
noExtField  :: NoExt
noExtField = noExt
#endif
