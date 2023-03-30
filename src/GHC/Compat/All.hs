{-# LANGUAGE CPP #-}
module GHC.Compat.All (
module X,
-- * Extras
mkFunTy,
mkLocalMultId,
) where

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
import GHC.Driver.Session       as X
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

import GHC.Core.Type            as X hiding (mkFunTy)
import GHC.Driver.Env.Types     as X
import GHC.Driver.Monad         as X
import GHC.Driver.Ppr           as X
import GHC.Hs                   as X hiding (FunDep, AnnRec, AnnLam, AnnCase, AnnLet, AnnType)
import GHC.Types.Fixity         as X
import GHC.Types.SourceText     as X
import GHC.Unit.Finder          as X
import GHC.Utils.Logger         as X

-------------------------------------------------------------------------------
-- Compat functions
-------------------------------------------------------------------------------

mkFunTy :: X.Type -> X.Type -> X.Type
mkFunTy =
    GHC.mkFunTy X.VisArg X.Many

mkLocalMultId :: X.Name -> X.Type -> X.Id
mkLocalMultId n t = X.mkLocalId n X.Many t
