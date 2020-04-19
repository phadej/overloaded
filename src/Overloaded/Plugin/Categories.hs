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
    :: Names
    -> Map GHC.Name b
    -> LHsExpr GhcRn
    -> Rewrite (Expression (Var b a))
parseExpr names ctx (L _ (HsPar _ expr)) =
    parseExpr names ctx expr
parseExpr _     ctx (L _ (HsVar _ (L l name)))
    | name == GHC.getName (GHC.tupleDataCon GHC.Boxed 0)
    = return ExpressionUnit
    | otherwise
    = case Map.lookup name ctx of
        Nothing -> Error $ \dflags ->
            putError dflags l $ GHC.text "Overloaded:Categories: Unbound variable" GHC.<+> GHC.ppr name
        Just b -> return $ ExpressionVar (B b)
parseExpr names ctx (L _ (ExplicitTuple _ [L _ (Present _ x), L _ (Present _ y)] Plugins.Boxed)) = do
    x' <- parseExpr names ctx x
    y' <- parseExpr names ctx y
    return (ExpressionTuple x' y')
parseExpr _     _ (L l ExplicitTuple {}) = Error $ \dflags ->
    putError dflags l $ GHC.text "Overloaded:Categories: only boxed tuples of arity 2 are supported"
parseExpr names ctx (L _ (HsApp _ (L _ (HsVar _ (L l fName))) x))
    | fName == conLeftName names = do
        x' <- parseExpr names ctx x
        return (ExpressionLeft x')
    | fName == conRightName names = do
        x' <- parseExpr names ctx x
        return (ExpressionRight x')
    | otherwise = Error $ \dflags ->
        putError dflags l $ GHC.text "Overloaded:Categories: only applications of Left and Right are supported"
parseExpr _     _   (L l expr) = Error $ \dflags ->
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
    expr' <- parseExpr names ctx expr
    return $ Last (Right morp') expr'
parseCmd names ctx (L _ (HsCmdArrApp _ morp expr HsHigherOrderApp _)) = do
    morp' <- parseExpr names ctx morp
    expr' <- parseExpr names ctx expr
    return $ Last (Left morp') expr'
parseCmd names ctx (L _ (HsCmdCase _ expr matchGroup)) =
    case mg_alts matchGroup of
#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(8,10,1)
        L _ [ L _ Match { m_pats = [XPat (L _ (ConPatIn (L _ acon) aargs))], m_grhss = abody' }
            , L _ Match { m_pats = [XPat (L _ (ConPatIn (L _ bcon) bargs))], m_grhss = bbody' }
            ]
#else
        L _ [ L _ Match { m_pats = [L _ (ConPatIn (L _ acon) aargs)], m_grhss = abody' }
            , L _ Match { m_pats = [L _ (ConPatIn (L _ bcon) bargs)], m_grhss = bbody' }
            ]
#endif
            -- Left and Right, or Right and Left
            |  [acon,bcon] == [conLeftName names,conRightName names]
            || [acon,bcon] == [conRightName names,conLeftName names]
            -- only one argument
            , [aarg] <- hsConPatArgs aargs
            , [barg] <- hsConPatArgs bargs
            -- and simple bodies
            , Just abody <- simpleGRHSs abody'
            , Just bbody <- simpleGRHSs bbody'

            -> do
                expr' <- parseExpr names ctx expr

                SomePattern apat <- parsePat aarg
                SomePattern bpat <- parsePat barg

                acont <- parseCmd names (combineMaps ctx apat) abody
                bcont <- parseCmd names (combineMaps ctx bpat) bbody

                -- Error $ \dflags -> putError dflags noSrcSpan $ GHC.text "TODO"
                --     GHC.$$ GHC.ppr acon
                --     GHC.$$ GHC.ppr bcon
                --     GHC.$$ GHC.ppr aarg
                --     GHC.$$ GHC.ppr barg
                --     GHC.$$ GHC.ppr abody
                --     GHC.$$ GHC.ppr bbody

                return $ caseCont expr' apat bpat (second assoc acont) (second assoc bcont)

        L l _ -> Error $ \dflags ->
            putError dflags l $ GHC.text "Overloaded:Categories only case of Left and Right are supported"
                GHC.$$ GHC.text (SYB.gshow (mg_alts matchGroup))
parseCmd _     _   (L l cmd) =
    Error $ \dflags ->
        putError dflags l $ GHC.text "Unsupported command in proc for Overloaded:Categories"
            GHC.$$ GHC.ppr cmd
            GHC.$$ GHC.text (SYB.gshow cmd)

simpleGRHSs :: GRHSs GhcRn body -> Maybe body
simpleGRHSs (GRHSs _ [L _ (GRHS _ [] body)] (L _ (EmptyLocalBinds _))) = Just body
simpleGRHSs _ = Nothing

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
    Last :: Either (Expression a) (Morphism term) -> Expression a -> Continuation term a
      -- ^ term -< y
    Edge
        :: Pattern sh String
        -> Either (Expression a) (Morphism term)
        -> Expression a
        -> Continuation term (Var (Index sh) a)
        -> Continuation term a
      -- ^ x <- term -< y

    Split
        :: Expression a
        -> Pattern shA String
        -> Pattern shB String
        -> Continuation term (Var (Index shA) a)
        -> Continuation term (Var (Index shB) a)
        -> Continuation term a

deriving instance (Show a, Show term) => Show (Continuation term a)

instance Bifunctor Continuation where
    bimap f g (Last term e)         = Last (bimap (fmap g) (fmap f) term) (fmap g e)
    bimap f g (Edge p term e c)     = Edge p (bimap (fmap g) (fmap f) term) (fmap g e) (bimap f (fmap g) c)
    bimap f g (Split e pa pb ca cb) = Split (fmap g e) pa pb
        (bimap f (fmap g) ca)
        (bimap f (fmap g) cb)

instance Functor (Continuation term) where
    fmap = second

compCont
    :: Pattern sh String
    -> Continuation term a
    -> Continuation term (Var (Index sh) a)
    -> Continuation term a
compCont pat (Last term expr) c
    = Edge pat term expr c
compCont pat (Edge pat' term expr c') c
    = Edge pat' term expr
    $ compCont pat c' (weaken1 c)
compCont pat (Split expr patA patB contA contB) c
    = Split expr patA patB
        (compCont pat contA (weaken1 c))
        (compCont pat contB (weaken1 c))

weaken1 :: Functor f => f (Var a b) -> f (Var a (Var c b))
weaken1 = fmap (unvar B (F . F))

caseCont
    :: Expression a
    -> Pattern shA Plugins.Name
    -> Pattern shB Plugins.Name
    -> Continuation (LHsExpr GhcRn) (Var (Index shA) a)
    -> Continuation (LHsExpr GhcRn) (Var (Index shB) a)
    -> Continuation (LHsExpr GhcRn) a
caseCont e patA patB =
    Split e (fmap nameToString patA) (fmap nameToString patB)

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
    | ExpressionUnit
    | ExpressionTuple (Expression a) (Expression a)
    | ExpressionLeft (Expression a)
    | ExpressionRight (Expression a)
  deriving (Show, Functor)

-------------------------------------------------------------------------------
-- Skeleton of syntax we desugar arrow notation to
-------------------------------------------------------------------------------

-- | Note: morpisms don't have variables!
data Morphism term
    = MId
    | MCompose (Morphism term) (Morphism term)
    | MProduct (Morphism term) (Morphism term)
    | MTerminal
    | MProj1
    | MProj2
    | MInL
    | MInR
    | MCase (Morphism term) (Morphism term)
    | MDistr
    | MEval
    | MTerm term
  deriving (Show, Functor)

instance Semigroup (Morphism term) where
    MTerminal <> _            = MTerminal
    MId       <> m            = m
    m         <> MId          = m
    MProj1    <> MProduct f _ = f
    MProj2    <> MProduct _ g = g
    MCase f _ <> MInL         = f
    MCase _ g <> MInR         = g
    f         <> g            = MCompose f g

instance Monoid (Morphism term) where
    mempty  = MId
    mappend = (<>)

-------------------------------------------------------------------------------
-- Desugaring
-------------------------------------------------------------------------------

desugar :: (a -> Morphism term) -> Proc term a -> Morphism term
desugar ctx (Proc p k) = desugarC (unvar (desugarP p) ctx) k

desugarC :: (a -> Morphism term) -> Continuation term a -> Morphism term
desugarC ctx (Last (Right term) e) = mconcat
    [ term
    , desugarE ctx e
    ]
desugarC ctx (Last (Left f) e) = mconcat
    [ MEval
    , MProduct (desugarE ctx f) (desugarE ctx e)
    ]
desugarC ctx (Edge p (Right term) e k) = mconcat
    [ desugarC (unvar (\x -> desugarP p x <> MProj1) (\y -> ctx y <> MProj2)) k
    , MProduct
        (term <> desugarE ctx e)
        MId
    ]
desugarC ctx (Edge p (Left f) e k) = mconcat
    [ desugarC (unvar (\x -> desugarP p x <> MEval <> MProj1) (\y -> ctx y <> MProj2)) k
    , MProduct
        (MProduct (desugarE ctx f) (desugarE ctx e))
        MId
    ]
desugarC ctx (Split e pa pb ka kb) = mconcat
    [ MCase
        (desugarC (unvar (\x -> desugarP pa x <> MProj1) (\y -> ctx y <> MProj2)) ka)
        (desugarC (unvar (\x -> desugarP pb x <> MProj1) (\y -> ctx y <> MProj2)) kb)
    , MDistr
    , MProduct
        (desugarE ctx e)
        MId
    ]

desugarP :: Pattern sh name -> Index sh -> Morphism term
desugarP (PatternVar _)     Here    = MId
desugarP PatternWild        Here    = MId
desugarP (PatternTuple l _) (InL i) = desugarP l i <> MProj1
desugarP (PatternTuple _ r) (InR i) = desugarP r i <> MProj2

desugarE :: (a -> Morphism term) -> Expression a -> Morphism term
desugarE ctx = go where
    go ExpressionUnit        = MTerminal
    go (ExpressionVar a)     = ctx a
    go (ExpressionTuple x y) = MProduct (go x) (go y)
    go (ExpressionLeft x)    = MInL <> go x
    go (ExpressionRight y)   = MInR <> go y

-------------------------------------------------------------------------------
-- Generating
-------------------------------------------------------------------------------

generate :: Names -> Morphism (LHsExpr GhcRn) -> LHsExpr GhcRn
generate Names {..} = go where
    go MId            = hsVar noSrcSpan catIdentityName
    go (MCompose f g) = hsPar noSrcSpan $ hsOpApp noSrcSpan (go f) (hsVar noSrcSpan catComposeName) (go g)
    go (MTerm term)   = term
    go MTerminal      = hsVar noSrcSpan catTerminalName
    go MProj1         = hsVar noSrcSpan catProj1Name
    go MProj2         = hsVar noSrcSpan catProj2Name
    go (MProduct f g) = hsPar noSrcSpan $ hsApps noSrcSpan (hsVar noSrcSpan catFanoutName) [go f, go g]
    go MInL           = hsVar noSrcSpan catInlName
    go MInR           = hsVar noSrcSpan catInrName
    go MDistr         = hsVar noSrcSpan catDistrName
    go MEval          = hsVar noSrcSpan catEvalName
    go (MCase f g)    = hsPar noSrcSpan $ hsApps noSrcSpan (hsVar noSrcSpan catFaninName) [go f, go g]
