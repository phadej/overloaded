{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Overloaded.Plugin.Categories where

import Data.Bifunctor       (Bifunctor (..))
import Data.Bifunctor.Assoc (Assoc (..))
import Data.Kind            (Type)
import Data.Map.Strict      (Map)
import Data.Void            (Void, absurd)

import qualified Data.Generics   as SYB
import qualified Data.Map.Strict as Map
import qualified GHC.Compat.All  as GHC
import           GHC.Compat.Expr
import qualified GhcPlugins      as Plugins

import Overloaded.Plugin.Diagnostics
import Overloaded.Plugin.Names
import Overloaded.Plugin.Rewrite

-------------------------------------------------------------------------------
-- Rewriter
-------------------------------------------------------------------------------

transformCategories
    :: Names
    -> LHsExpr GhcRn
    -> Rewrite (LHsExpr GhcRn)
transformCategories names (L _l (HsProc _ pat (L _ (HsCmdTop _ cmd)))) = do
    SomePattern pat' <- parsePat pat
    kont <- parseCmd names (patternMap pat') cmd
    let proc :: Proc (LHsExpr GhcRn) Void
        proc = Proc (nameToString <$> pat') kont

        morp :: Morphism (LHsExpr GhcRn)
        morp = desugar absurd proc

        expr :: LHsExpr GhcRn
        expr = generate names morp

    -- _ <- Error $ \dflags -> putError dflags _l $ GHC.text "DEBUG"
    --     GHC.$$ GHC.text (show $ first (GHC.showPpr dflags) proc)
    --     GHC.$$ GHC.text (show $ fmap  (GHC.showPpr dflags) morp)
    --     GHC.$$ GHC.ppr expr

    return expr

transformCategories _ _ = NoRewrite

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

parsePat :: LPat GhcRn -> Rewrite (SomePattern GHC.Name)
#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(8,10,1)
parsePat (XPat (L l pat)) = parsePat' l pat
parsePat pat              = parsePat' noSrcSpan pat
#else
parsePat (L l pat) = parsePat' l pat
#endif

parsePat' :: SrcSpan -> Pat GhcRn -> Rewrite (SomePattern GHC.Name)
parsePat' _ WildPat {} =
    return $ SomePattern PatternWild
parsePat' _ (VarPat _ (L _ name)) =
    return $ SomePattern $ PatternVar name
parsePat' _ (TuplePat _ [x, y] Plugins.Boxed) = do
    SomePattern x' <- parsePat x
    SomePattern y' <- parsePat y
    return $ SomePattern $ PatternTuple x' y'
parsePat' l TuplePat {} = Error $ \dflags ->
    putError dflags l $ GHC.text "Overloaded:Categories: only boxed tuples of arity 2 are supported"
parsePat' l pat = Error $ \dflags ->
    putError dflags l $ GHC.text "Cannot parse pattern for Overloaded:Categories"
        GHC.$$ GHC.ppr pat
        GHC.$$ GHC.text (SYB.gshow pat)

parseExpr
    :: Map GHC.Name b
    -> LHsExpr GhcRn
    -> Rewrite (Expression (Var b a))
parseExpr ctx (L _ (HsVar _ (L l name))) =
    case Map.lookup name ctx of
        Nothing -> Error $ \dflags ->
            putError dflags l $ GHC.text "Unbound variable" GHC.<+> GHC.ppr name
        Just b -> return $ ExpressionVar (B b)
parseExpr ctx (L _ (ExplicitTuple _ [L _ (Present _ x), L _ (Present _ y)] Plugins.Boxed)) = do
    x' <- parseExpr ctx x
    y' <- parseExpr ctx y
    return (ExpressionTuple x' y')
parseExpr _ (L l ExplicitTuple {}) = Error $ \dflags ->
    putError dflags l $ GHC.text "Overloaded:Categories: only boxed tuples of arity 2 are supported"
parseExpr _   (L l expr) = Error $ \dflags ->
    putError dflags l $ GHC.text "Cannot parse -< right-hand-side for Overloaded:Categories"
        GHC.$$ GHC.ppr expr
        GHC.$$ GHC.text (SYB.gshow expr)

parseCmd
    :: Names
    -> Map GHC.Name b
    -> LHsCmd GhcRn
    -> Rewrite (Continuation (LHsExpr GhcRn) (Var b a))
parseCmd names ctx (L _ (HsCmdDo _ (L l stmts))) =
    parseStmts names ctx l stmts
parseCmd names ctx (L _ (HsCmdArrApp _ morp expr HsFirstOrderApp _)) = do
    morp' <- parseTerm names morp
    expr' <- parseExpr ctx expr
    return $ Last morp' expr'
parseCmd _     _   (L l cmd) =
    Error $ \dflags ->
        putError dflags l $ GHC.text "Unsupported command in proc for Overloaded:Categories"
            GHC.$$ GHC.ppr cmd
            GHC.$$ GHC.text (SYB.gshow cmd)

parseTerm
    :: Names
    -> LHsExpr GhcRn
    -> Rewrite (Morphism (LHsExpr GhcRn))
parseTerm Names {..} (L _ (HsVar _ (L _ name)))
    | name == catIdentityName = return MId
parseTerm _ term = return (MTerm term)

parseStmts
    :: Names
    -> Map GHC.Name b
    -> SrcSpan
    -> [CmdLStmt GhcRn]
    -> Rewrite (Continuation (LHsExpr GhcRn) (Var b a))
parseStmts names ctx _ (L l (BindStmt _ pat body _ _) : next) = do
    SomePattern pat' <- parsePat pat
    cont1 <- parseCmd names ctx body
    cont2 <- parseStmts names (combineMaps ctx pat') l next
    return $ compCont (nameToString <$> pat') cont1 (second assoc cont2)
parseStmts names ctx _ [L _ (LastStmt _ body _ _)] =
    parseCmd names ctx body
parseStmts _     _   _ (L l stmt : _) =
    Error $ \dflags ->
        putError dflags l $ GHC.text "Unsupported statement in proc-do for Overloaded:Categories"
            GHC.$$ GHC.ppr stmt
            GHC.$$ GHC.text (SYB.gshow stmt)
parseStmts _     _   l [] =
    Error $ \dflags ->
        putError dflags l $ GHC.text "Empty do block in proc"

-------------------------------------------------------------------------------
-- Variables
-------------------------------------------------------------------------------

data Var b a
    = B b
    | F a
  deriving (Show, Functor)

instance Bifunctor Var where
    bimap f _ (B b) = B (f b)
    bimap _ g (F a) = F (g a)

instance Assoc Var where
    assoc (B (B x)) = B x
    assoc (B (F y)) = F (B y)
    assoc (F z)     = F (F z)

    unassoc (B x)     = B (B x)
    unassoc (F (B y)) = B (F y)
    unassoc (F (F z)) = F z

unvar :: (b -> c) -> (a -> c) -> Var b a -> c
unvar f _ (B b) = f b
unvar _ g (F a) = g a

-------------------------------------------------------------------------------
-- A subset of Arrow notation syntax we support.
-------------------------------------------------------------------------------

-- | Proc syntax
data Proc term a where
    Proc :: Pattern sh String -> Continuation term (Var (Index sh) a) -> Proc term a

deriving instance (Show a, Show term) => Show (Proc term a)

instance Bifunctor Proc where
    bimap f g (Proc p c) = Proc p (bimap f (fmap g) c)

data Continuation term a where
    Last :: Morphism term -> Expression a -> Continuation term a
      -- ^ term -< y
    Edge
        :: Pattern sh String
        -> Morphism term
        -> Expression a
        -> Continuation term (Var (Index sh) a)
        -> Continuation term a
      -- ^ x <- term -< y

deriving instance (Show a, Show term) => Show (Continuation term a)

instance Bifunctor Continuation where
    bimap f g (Last term e)     = Last (fmap f term) (fmap g e)
    bimap f g (Edge p term e c) = Edge p (fmap f term) (fmap g e) (bimap f (fmap g) c)

compCont
    :: Pattern sh String
    -> Continuation term a
    -> Continuation term (Var (Index sh) a)
    -> Continuation term a
compCont pat (Last term expr) c
    = Edge pat term expr c
compCont pat (Edge pat' term expr c') c
    = Edge pat' term expr
    $ compCont pat c' (second (unvar B (F . F)) c)

-------------------------------------------------------------------------------
-- Patterns
-------------------------------------------------------------------------------

data Shape = One | Two Shape Shape

data Pattern :: Shape -> Type -> Type where
    PatternVar   :: a -> Pattern 'One a
    PatternWild  :: Pattern 'One a
    PatternTuple :: Pattern l a -> Pattern r a -> Pattern ('Two l r) a

deriving instance Show a => Show (Pattern sh a)
deriving instance Functor (Pattern sh)

data SomePattern :: Type -> Type where
    SomePattern :: Pattern sh a -> SomePattern a

data Index :: Shape -> Type where
    Here :: Index 'One
    InL  :: Index x -> Index ('Two x y)
    InR  :: Index y -> Index ('Two x y)

deriving instance Show (Index sh)

patternMap :: Ord a => Pattern sh a -> Map a (Index sh)
patternMap (PatternVar x)     = Map.singleton x Here
patternMap PatternWild        = Map.empty
patternMap (PatternTuple l r) = Map.union
    (Map.map InL (patternMap l))
    (Map.map InR (patternMap r))

combineMaps
    :: Map Plugins.Name b
    -> Pattern sh Plugins.Name
    -> Map Plugins.Name (Var (Index sh) b)
combineMaps m pat = Map.union (Map.map F m) (Map.map B (patternMap pat))

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

data Expression a
    = ExpressionVar a
    | ExpressionTuple (Expression a) (Expression a)
  deriving (Show, Functor)

-------------------------------------------------------------------------------
-- Skeleton of syntax we desugar arrow notation to
-------------------------------------------------------------------------------

-- | Note: morpisms don't have variables!
data Morphism term
    = MId
    | MCompose (Morphism term) (Morphism term)
    | MProduct (Morphism term) (Morphism term)
    | MProj1
    | MProj2
    | MTerm term
  deriving (Show, Functor)

instance Semigroup (Morphism term) where
    MId <> m = m
    m <> MId = m
    MProj1 <> MProduct f _ = f
    MProj2 <> MProduct _ g = g
    f <> g   = MCompose f g

instance Monoid (Morphism term) where
    mempty  = MId
    mappend = (<>)

-------------------------------------------------------------------------------
-- Desugaring
-------------------------------------------------------------------------------

desugar :: (a -> Morphism term) -> Proc term a -> Morphism term
desugar ctx (Proc p k) = desugarC (unvar (desugarP p) ctx) k

desugarC :: (a -> Morphism term) -> Continuation term a -> Morphism term
desugarC ctx (Last term e) =
    term <> desugarE ctx e
desugarC ctx (Edge p term e k) = mconcat
    [ desugarC (unvar (\x -> desugarP p x <> MProj1) (\y -> ctx y <> MProj2)) k
    , MProduct
        (term <> desugarE ctx e)
        MId
    ]

desugarP :: Pattern sh name -> Index sh -> Morphism term
desugarP (PatternVar _)     Here    = MId
desugarP PatternWild        Here    = MId
desugarP (PatternTuple l _) (InL i) = desugarP l i <> MProj1
desugarP (PatternTuple _ r) (InR i) = desugarP r i <> MProj2

desugarE :: (a -> Morphism term) -> Expression a -> Morphism term
desugarE ctx = go where
    go (ExpressionVar a) = ctx a
    go (ExpressionTuple x y) = MProduct (go x) (go y)

-------------------------------------------------------------------------------
-- Generating
-------------------------------------------------------------------------------

generate :: Names -> Morphism (LHsExpr GhcRn) -> LHsExpr GhcRn
generate Names {..} = go where
    go MId            = hsVar noSrcSpan catIdentityName
    go (MCompose f g) = hsPar noSrcSpan $ hsOpApp noSrcSpan (go f) (hsVar noSrcSpan catComposeName) (go g)
    go (MTerm term)   = term
    go MProj1         = hsVar noSrcSpan catProj1Name
    go MProj2         = hsVar noSrcSpan catProj2Name
    go (MProduct f g) = hsPar noSrcSpan $ hsApps noSrcSpan (hsVar noSrcSpan catFanoutName) [go f, go g]
