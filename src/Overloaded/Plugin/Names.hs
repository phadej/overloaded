{-# LANGUAGE RecordWildCards #-}
module Overloaded.Plugin.Names (
    -- * Names
    Names (..),
    getNames,
    -- * CatNames
    CatNames (..),
    getCatNames,
    -- * RrdNames
    RdrNames (..),
    getRdrNames,
    -- * VarName
    VarName (..),
    lookupVarName,
    lookupTypeName,
    -- * RdrName
    mkRdrName,
    -- * Selected modules
    ghcRecordsCompatMN,
    overloadedConstructorsMN,
    ) where

import Control.Monad.IO.Class (MonadIO (..))

import Overloaded.Plugin.Diagnostics

import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr

data Names = Names
    { fromStringName     :: GHC.Name
    , fromSymbolName     :: GHC.Name
    , fromNumeralName    :: GHC.Name
    , fromNaturalName    :: GHC.Name
    , fromCharName       :: GHC.Name
    , nilName            :: GHC.Name
    , consName           :: GHC.Name
    , ifteName           :: GHC.Name
    , unitName           :: GHC.Name
    , fromLabelName      :: GHC.Name
    , fromTypeNatName    :: GHC.Name
    , fromTypeSymbolName :: GHC.Name
    , fmapName           :: GHC.Name
    , pureName           :: GHC.Name
    , apName             :: GHC.Name
    , birdName           :: GHC.Name
    , voidName           :: GHC.Name
    , composeName        :: GHC.Name
    , doPureName         :: GHC.Name
    , doThenName         :: GHC.Name
    , doBindName         :: GHC.Name
    , conLeftName        :: GHC.Name
    , conRightName       :: GHC.Name
    , codeFromLabelName  :: GHC.Name
    , codeFromStringName :: GHC.Name
    , catNames           :: CatNames
    }

data RdrNames = RdrNames
    { dollarName         :: GHC.RdrName
    , buildName          :: GHC.RdrName
    }

data CatNames = CatNames
    { catIdentityName    :: GHC.Name
    , catComposeName     :: GHC.Name
    , catTerminalName    :: GHC.Name
    , catProj1Name       :: GHC.Name
    , catProj2Name       :: GHC.Name
    , catFanoutName      :: GHC.Name
    , catInlName         :: GHC.Name
    , catInrName         :: GHC.Name
    , catFaninName       :: GHC.Name
    , catDistrName       :: GHC.Name
    , catEvalName        :: GHC.Name
    }

getNames :: GHC.DynFlags -> GHC.HscEnv -> GHC.TcM Names
getNames dflags env = do
    fromStringName  <- lookupName dflags env dataStringMN "fromString"
    fromSymbolName  <- lookupName dflags env overloadedSymbolsMN "fromSymbol"
    fromNumeralName <- lookupName dflags env overloadedNumeralsMN "fromNumeral"
    fromNaturalName <- lookupName dflags env overloadedNaturalsMN "fromNatural"
    fromCharName    <- lookupName dflags env overloadedCharsMN "fromChar"
    nilName         <- lookupName dflags env overloadedListsMN "nil"
    unitName        <- lookupName dflags env overloadedListsMN "nil"
    consName        <- lookupName dflags env overloadedListsMN "cons"
    ifteName        <- lookupName dflags env overloadedIfMN "ifte"
    fromLabelName   <- lookupName dflags env ghcOverloadedLabelsMN "fromLabel"

    fromTypeNatName    <- lookupName' dflags env overloadedTypeNatsMN "FromNat"
    fromTypeSymbolName <- lookupName' dflags env overloadedTypeSymbolsMN "FromTypeSymbol"

    fmapName <- lookupName dflags env ghcBaseMN "fmap"
    pureName <- lookupName dflags env ghcBaseMN "pure"
    apName   <- lookupName dflags env ghcBaseMN "<*>"
    birdName <- lookupName dflags env ghcBaseMN "<*"
    voidName <- lookupName dflags env dataFunctorMN "void"

    composeName <- lookupName dflags env ghcBaseMN "."

    doPureName <- lookupName' dflags env overloadedDoMN "Pure"
    doBindName <- lookupName' dflags env overloadedDoMN "Bind"
    doThenName <- lookupName' dflags env overloadedDoMN "Then"

    conLeftName  <- lookupNameDataCon dflags env dataEitherMN "Left"
    conRightName <- lookupNameDataCon dflags env dataEitherMN "Right"

    codeFromLabelName  <- lookupName dflags env overloadedCodeLabelsMN  "codeFromLabel"
    codeFromStringName <- lookupName dflags env overloadedCodeStringsMN "codeFromString"


    catNames <- getCatNames dflags env overloadedCategoriesMN

    return Names {..}

getRdrNames :: GHC.DynFlags -> GHC.HscEnv -> GHC.Hsc RdrNames
getRdrNames dflags env = do
    let dollarName = GHC.Exact GHC.dollarName
    buildName <- GHC.Exact <$> lookupName dflags env overloadedConstructorsMN "build"

    return RdrNames {..}

getCatNames :: GHC.DynFlags -> GHC.HscEnv -> GHC.ModuleName -> GHC.TcM CatNames
getCatNames dflags env module_ = do
    catIdentityName <- lookupName dflags env module_ "identity"
    catComposeName  <- lookupName dflags env module_ "%%"
    catProj1Name    <- lookupName dflags env module_ "proj1"
    catProj2Name    <- lookupName dflags env module_ "proj2"
    catFanoutName   <- lookupName dflags env module_ "fanout"
    catInlName      <- lookupName dflags env module_ "inl"
    catInrName      <- lookupName dflags env module_ "inr"
    catFaninName    <- lookupName dflags env module_ "fanin"
    catDistrName    <- lookupName dflags env module_ "distr"
    catEvalName     <- lookupName dflags env module_ "eval"
    catTerminalName <- lookupName dflags env module_ "terminal"

    return CatNames {..}

lookupName :: (GHC.HasLogger m, MonadIO m) => GHC.DynFlags -> GHC.HscEnv -> GHC.ModuleName -> String -> m GHC.Name
lookupName dflags env mn vn = do
    res <- liftIO $ GHC.findImportedModule env mn Nothing
    case res of
        GHC.Found _ md -> liftIO $ GHC.lookupOrigIO env md (GHC.mkVarOcc vn)
        _              -> do
            putError dflags noSrcSpan $ GHC.text "Cannot find module" GHC.<+> GHC.ppr mn
            liftIO $ fail "panic!"

lookupNameDataCon :: GHC.DynFlags -> GHC.HscEnv -> GHC.ModuleName -> String -> GHC.TcM GHC.Name
lookupNameDataCon dflags env mn vn = do
    res <-  liftIO $ GHC.findImportedModule env mn Nothing
    case res of
        GHC.Found _ md -> GHC.lookupOrig md (GHC.mkDataOcc vn)
        _              -> do
            putError dflags noSrcSpan $ GHC.text "Cannot find module" GHC.<+> GHC.ppr mn
            fail "panic!"

lookupName' :: GHC.DynFlags -> GHC.HscEnv -> GHC.ModuleName -> String -> GHC.TcM GHC.Name
lookupName' dflags env mn vn = do
    res <-  liftIO $ GHC.findImportedModule env mn Nothing
    case res of
        GHC.Found _ md -> GHC.lookupOrig md (GHC.mkTcOcc vn)
        _              -> do
            putError dflags noSrcSpan $ GHC.text "Cannot find module" GHC.<+> GHC.ppr mn
            fail "panic!"

-- | Module name and variable name
data VarName = VN String String
  deriving (Eq, Show)

lookupVarName :: GHC.DynFlags -> GHC.HscEnv -> VarName -> GHC.TcM GHC.Name
lookupVarName dflags env (VN vn mn) = lookupName dflags env (GHC.mkModuleName vn) mn

lookupTypeName :: GHC.DynFlags -> GHC.HscEnv -> VarName -> GHC.TcM GHC.Name
lookupTypeName dflags env (VN vn mn) = lookupName' dflags env (GHC.mkModuleName vn) mn

-- TODO: ignores module
mkRdrName :: VarName -> GHC.RdrName
mkRdrName (VN _ rn) = GHC.Unqual $ GHC.mkVarOcc rn

-------------------------------------------------------------------------------
-- ModuleNames
-------------------------------------------------------------------------------

dataStringMN :: GHC.ModuleName
dataStringMN =  GHC.mkModuleName "Data.String"

overloadedCharsMN :: GHC.ModuleName
overloadedCharsMN =  GHC.mkModuleName "Overloaded.Chars"

overloadedSymbolsMN :: GHC.ModuleName
overloadedSymbolsMN =  GHC.mkModuleName "Overloaded.Symbols"

overloadedNaturalsMN :: GHC.ModuleName
overloadedNaturalsMN =  GHC.mkModuleName "Overloaded.Naturals"

overloadedNumeralsMN :: GHC.ModuleName
overloadedNumeralsMN =  GHC.mkModuleName "Overloaded.Numerals"

overloadedListsMN :: GHC.ModuleName
overloadedListsMN =  GHC.mkModuleName "Overloaded.Lists"

overloadedIfMN :: GHC.ModuleName
overloadedIfMN =  GHC.mkModuleName "Overloaded.If"

overloadedDoMN :: GHC.ModuleName
overloadedDoMN =  GHC.mkModuleName "Overloaded.Do"

overloadedCategoriesMN :: GHC.ModuleName
overloadedCategoriesMN =  GHC.mkModuleName "Overloaded.Categories"

ghcOverloadedLabelsMN :: GHC.ModuleName
ghcOverloadedLabelsMN =  GHC.mkModuleName "GHC.OverloadedLabels"

overloadedCodeLabelsMN :: GHC.ModuleName
overloadedCodeLabelsMN =  GHC.mkModuleName "Overloaded.CodeLabels"

overloadedCodeStringsMN :: GHC.ModuleName
overloadedCodeStringsMN =  GHC.mkModuleName "Overloaded.CodeStrings"

overloadedTypeNatsMN :: GHC.ModuleName
overloadedTypeNatsMN =  GHC.mkModuleName "Overloaded.TypeNats"

overloadedTypeSymbolsMN :: GHC.ModuleName
overloadedTypeSymbolsMN =  GHC.mkModuleName "Overloaded.TypeSymbols"

overloadedConstructorsMN :: GHC.ModuleName
overloadedConstructorsMN =  GHC.mkModuleName "Overloaded.Constructors"

ghcRecordsCompatMN :: GHC.ModuleName
ghcRecordsCompatMN =  GHC.mkModuleName "GHC.Records.Compat"

ghcBaseMN :: GHC.ModuleName
ghcBaseMN = GHC.mkModuleName "GHC.Base"

dataFunctorMN :: GHC.ModuleName
dataFunctorMN = GHC.mkModuleName "Data.Functor"

dataEitherMN :: GHC.ModuleName
dataEitherMN = GHC.mkModuleName "Data.Either"
