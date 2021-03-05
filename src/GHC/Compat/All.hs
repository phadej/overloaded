{-# LANGUAGE CPP #-}
module GHC.Compat.All (
module X,
-- * Extras
mkFunTy,
) where

#if MIN_VERSION_ghc(8,10,0)
import Constraint as X
import Predicate  as X
import Type       as X
#else
import Type as X hiding (mkFunTy)
#endif

import BasicTypes as X
import Class      as X
import CoreSyn    as X
import DataCon    as X
import DynFlags   as X
import ErrUtils   as X
import FamInst    as X
import FamInstEnv as X
import Finder     as X
import GHC        as X (HscEnv)
import Id         as X
import IfaceEnv   as X
import MkCore     as X
import Module     as X
import Name       as X
import Outputable as X
import RdrName    as X
import SrcLoc     as X
import TcEnv      as X
import TcEvidence as X
import TcMType    as X
import TcRnMonad  as X
import TyCon      as X
import TyCoRep    as X hiding (mkFunTy)
import TysWiredIn as X

import qualified TyCoRep as GHC

-------------------------------------------------------------------------------
-- Compat functions
-------------------------------------------------------------------------------

mkFunTy :: X.Type -> X.Type -> X.Type
mkFunTy =
#if MIN_VERSION_ghc(8,10,0)
    GHC.mkFunTy X.VisArg
#else
    GHC.mkFunTy
#endif
