{-# LANGUAGE CPP #-}
module GHC.Compat.All (
module X,
-- * Extras
mkFunTy,
mkLocalMultId,
) where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Names        as X
import GHC.Builtin.Types        as X
import GHC.Core                 as X
import GHC.Core.Class           as X
import GHC.Core.Coercion        as X
import GHC.Core.DataCon         as X
import GHC.Core.FamInstEnv      as X
import GHC.Core.Make            as X
import GHC.Core.Predicate       as X
import GHC.Core.TyCon           as X
import GHC.Core.Type            as X
import GHC.Driver.Finder        as X
import GHC.Driver.Session       as X
import GHC.Driver.Types         as X
import GHC.Hs                   as X
import GHC.Iface.Env            as X
import GHC.Tc.Instance.Family   as X
import GHC.Tc.Types             as X
import GHC.Tc.Types.Constraint  as X
import GHC.Tc.Types.Evidence    as X
import GHC.Tc.Utils.Env         as X
import GHC.Tc.Utils.Instantiate as X
import GHC.Tc.Utils.Monad       as X
import GHC.Tc.Utils.TcMType     as X
import GHC.Types.Basic          as X
import GHC.Types.Id             as X
import GHC.Types.Name           as X
import GHC.Types.Name.Reader    as X
import GHC.Types.SrcLoc         as X
import GHC.Unit.Module.Name     as X
import GHC.Utils.Error          as X
import GHC.Utils.Outputable     as X

import GHC     as X (Module)

import qualified GHC.Core.TyCo.Rep as GHC

#else
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
import HscTypes   as X
import Id         as X
import IfaceEnv   as X
import MkCore     as X
import Module     as X
import Name       as X
import Outputable as X
import PrelNames  as X
import RdrName    as X
import SrcLoc     as X
import TcEnv      as X
import TcEvidence as X
import TcMType    as X
import TcRnMonad  as X
import TyCoRep    as X hiding (mkFunTy)
import TyCon      as X
import TysWiredIn as X

import qualified TyCoRep as GHC
#endif

-------------------------------------------------------------------------------
-- Compat functions
-------------------------------------------------------------------------------

mkFunTy :: X.Type -> X.Type -> X.Type
mkFunTy =
#if MIN_VERSION_ghc(9,0,0)
    GHC.mkFunTy X.VisArg X.Many
#elif MIN_VERSION_ghc(8,10,0)
    GHC.mkFunTy X.VisArg
#else
    GHC.mkFunTy
#endif

mkLocalMultId :: X.Name -> X.Type -> X.Id
#if MIN_VERSION_ghc(9,0,0)
mkLocalMultId n t = X.mkLocalId n X.Many t
#else
mkLocalMultId n t = X.mkLocalId n t
#endif
