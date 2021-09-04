{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase          #-}
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

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Plugins     as Plugins
#else
import qualified GhcPlugins      as Plugins
#endif

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
    Some pat' <- parsePat pat
    kont <- parseCmd names (patternMap pat') cmd
    let proc :: Proc (LHsExpr GhcRn) Void
        proc = Proc (mapPattern nameToString pat') kont

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
parsePat' _ (ParPat _ pat) =
    parsePat pat
parsePat' _ WildPat {} =
    return $ Some PatternWild
parsePat' _ (VarPat _ (L _ name)) =
    return $ Some $ PatternVar name
parsePat' _ (TuplePat _ [x, y] GHC.Boxed) = do
    Some x' <- parsePat x
    Some y' <- parsePat y
    return $ Some $ PatternTuple x' y'
parsePat' l TuplePat {} = Error $ \dflags ->
    putError dflags l $ GHC.text "Overloaded:Categories: only boxed tuples of arity 2 are supported"
parsePat' _ (ConPat _ (L _ dc) (GHC.PrefixCon ps)) = do
    Some ps' <- fromListNP <$> traverse parsePat ps 
    withNamesNP 0 ps' $ \ns' -> return $ Some $ PatternCon (WrapName dc) ns' ps'
parsePat' l pat = Error $ \dflags ->
    putError dflags l $ GHC.text "Cannot parse pattern for Overloaded:Categories"
        GHC.$$ GHC.ppr pat
        GHC.$$ GHC.text (SYB.gshow pat)

withNamesNP :: Int -> NP f xs -> (NP (K WrappedName) xs -> Rewrite a) -> Rewrite a
withNamesNP _ Nil       k = k Nil
withNamesNP i (_ :* xs) k =
    WithName ("arg" ++ show i) $ \n ->
    withNamesNP (i + 1) xs $ \ns ->
    k (K (WrapName n) :* ns)

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
parseExpr names ctx (L _ (ExplicitTuple _ [L _ (Present _ x), L _ (Present _ y)] GHC.Boxed)) = do
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
parseExpr names ctx expr
    | (L _ (HsVar _ (L _ dc)), xs) <- matchHsApps expr
    , GHC.isDataOcc (GHC.occName dc)
    = do
        xs' <- traverse (parseExpr names ctx) xs
        withNames 0 xs' $ \ns' -> return $
            ExpressionMany (WrapName dc) ns'
parseExpr _     _   (L l expr) = Error $ \dflags ->
    putError dflags l $ GHC.text "Cannot parse -< right-hand-side for Overloaded:Categories"
        GHC.$$ GHC.ppr expr
        GHC.$$ GHC.text (SYB.gshow expr)

withNames :: Int -> [x] -> ([(WrappedName, x)] -> Rewrite a) -> Rewrite a
withNames _ []       k = k []
withNames i (x : xs) k =
    WithName ("arg" ++ show i) $ \n ->
    withNames (i + 1) xs $ \ns ->
    k ((WrapName n, x) : ns)

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
#if MIN_VERSION_ghc(9,0,1)
        L _ [ L _ Match { m_pats = [L _ (ConPat _ (L _ acon) aargs)], m_grhss = abody' }
            , L _ Match { m_pats = [L _ (ConPat _ (L _ bcon) bargs)], m_grhss = bbody' }
            ]
#elif MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(8,10,1)
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

                Some apat <- parsePat aarg
                Some bpat <- parsePat barg

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
parseTerm Names {catNames = CatNames {..}} (L _ (HsVar _ (L _ name)))
    | name == catIdentityName = return MId
parseTerm _ term = return (MTerm term)

parseStmts
    :: Names
    -> Map GHC.Name b
    -> SrcSpan
    -> [CmdLStmt GhcRn]
    -> Rewrite (Continuation (LHsExpr GhcRn) (Var b a))
#if MIN_VERSION_ghc(9,0,1)
parseStmts names ctx _ (L l (BindStmt _ pat body) : next) = do
#else
parseStmts names ctx _ (L l (BindStmt _ pat body _ _) : next) = do
#endif
    Some pat' <- parsePat pat
    cont1 <- parseCmd names ctx body
    cont2 <- parseStmts names (combineMaps ctx pat') l next
    return $ compCont (mapPattern nameToString pat') cont1 (second assoc cont2)
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
    Proc :: Pattern String sh -> Continuation term (Var (Index sh) a) -> Proc term a

deriving instance (Show a, Show term) => Show (Proc term a)

instance Bifunctor Proc where
    bimap f g (Proc p c) = Proc p (bimap f (fmap g) c)

data Continuation term a where
    Last :: Either (Expression a) (Morphism term) -> Expression a -> Continuation term a
      -- ^ last: @term -< y@
  
    Edge
        :: Pattern String sh
        -> Either (Expression a) (Morphism term)
        -> Expression a
        -> Continuation term (Var (Index sh) a)
        -> Continuation term a
      -- ^ bind: @x <- term -< y@

    Split
        :: Expression a
        -> Pattern String shA
        -> Pattern String shB
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
    :: Pattern String sh
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
    -> Pattern Plugins.Name shA
    -> Pattern Plugins.Name shB
    -> Continuation (LHsExpr GhcRn) (Var (Index shA) a)
    -> Continuation (LHsExpr GhcRn) (Var (Index shB) a)
    -> Continuation (LHsExpr GhcRn) a
caseCont e patA patB =
    Split e (mapPattern nameToString patA) (mapPattern nameToString patB)

-------------------------------------------------------------------------------
-- Showable GHC Name
-------------------------------------------------------------------------------

newtype WrappedName = WrapName { unwrapName :: GHC.Name }

instance Show WrappedName where
    showsPrec _(WrapName n) = shows (nameToString n)

-------------------------------------------------------------------------------
-- Patterns
-------------------------------------------------------------------------------

data Shape = One | Two Shape Shape | Many [Shape]

data Pattern :: Type -> Shape -> Type where
    PatternVar   :: a -> Pattern a 'One
    PatternWild  :: Pattern a 'One
    PatternTuple
        :: Pattern a l
        -> Pattern a r
        -> Pattern a ('Two l r)
    PatternCon
        :: WrappedName
        -> NP (K WrappedName) xs
        -> NP (Pattern a) xs
        -> Pattern a ('Many xs)

deriving instance Show a => Show (Pattern a sh)
instance Show a => ShowU (Pattern a) where showsPrecU = showsPrec

mapPattern :: (a -> b) -> Pattern a sh -> Pattern b sh
mapPattern f (PatternVar x)        = PatternVar (f x)
mapPattern _ PatternWild           = PatternWild
mapPattern f (PatternTuple l r)    = PatternTuple (mapPattern f l) (mapPattern f r)
mapPattern f (PatternCon dc ns ps) = PatternCon dc ns (mapNP (mapPattern f) ps)

data Some (f :: k -> Type) where
    Some :: f a -> Some f

type SomePattern a = Some (Pattern a)

data Index :: Shape -> Type where
    Here :: Index 'One
    InL  :: Index x -> Index ('Two x y)
    InR  :: Index y -> Index ('Two x y)
    Pick :: NS Index xs -> Index ('Many xs)

deriving instance Show (Index sh)
instance ShowU Index where showsPrecU = showsPrec

patternMap :: Ord k => Pattern k sh -> Map k (Index sh)
patternMap (PatternVar x)     = Map.singleton x Here
patternMap PatternWild        = Map.empty
patternMap (PatternTuple l r) = Map.union
    (Map.map InL (patternMap l))
    (Map.map InR (patternMap r))
patternMap (PatternCon _ _ ps)  =
    Map.map Pick $ Map.unions $ go ps
  where
    go :: Ord k => NP (Pattern k) ps -> [Map k (NS Index ps)]
    go Nil = []
    go (x :* xs) = Map.map Z (patternMap x) : map (Map.map S) (go xs)

combineMaps :: Ord k => Map k b -> Pattern k sh -> Map k (Var (Index sh) b)
combineMaps m pat = Map.union (Map.map F m) (Map.map B (patternMap pat))

-------------------------------------------------------------------------------
-- mini-sop
-------------------------------------------------------------------------------

class ShowU (f :: k -> Type) where
    showsPrecU :: Int -> f a -> ShowS

data NP (f :: k -> Type) (xs :: [k]) where
    Nil  :: NP f '[]
    (:*) :: f x -> NP f xs -> NP f (x ': xs)

infixr 5 :*

toListNP :: (NP (K a) xs) -> [a]
toListNP Nil         = []
toListNP (K x :* xs) = x : toListNP xs

fromListNP :: [Some f] -> Some (NP f)
fromListNP [] = Some Nil
fromListNP (Some x : xs) = case fromListNP xs of
    Some xs' -> Some (x :* xs')

mapNP :: (forall x. f x -> g x) -> NP f xs -> NP g xs
mapNP _ Nil       = Nil
mapNP f (x :* xs) = f x :* mapNP f xs

instance ShowU f => Show (NP f xs) where
    showsPrec _ Nil = showString "Nil"
    showsPrec d (x :* xs) = showParen (d > 5)
        $ showsPrecU 6 x
        . showString " :* "
        . showsPrec 5 xs

data NS (f :: k -> Type) (xs :: [k]) where
    Z :: f x -> NS f (x ': xs)
    S :: NS f xs -> NS f (x ': xs)

instance ShowU f => Show (NS f xs) where
    showsPrec d (Z x) = showParen (d > 10)
        $ showString "Z " . showsPrecU 11 x
    showsPrec d (S x) = showParen (d > 10)
        $ showString "S " . showsPrec 11 x

newtype K a b = K a
  deriving (Eq, Show)

instance Show a => ShowU (K a) where showsPrecU = showsPrec

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

data Expression :: Type -> Type where
    ExpressionVar   :: a -> Expression a
    ExpressionUnit  :: Expression a
    ExpressionTuple :: Expression a -> Expression a -> Expression a
    ExpressionLeft  :: Expression a -> Expression a
    ExpressionRight :: Expression a -> Expression a
    ExpressionMany  :: WrappedName -> [(WrappedName, Expression a)] -> Expression a
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
    | MPat WrappedName [WrappedName]
    | MExp WrappedName [WrappedName]
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

desugarP :: Pattern name sh -> Index sh -> Morphism term
desugarP (PatternVar _)          Here      = MId
desugarP PatternWild             Here      = MId
desugarP (PatternTuple l _)      (InL i)   = desugarP l i <> MProj1
desugarP (PatternTuple _ r)      (InR i)   = desugarP r i <> MProj2
desugarP (PatternCon dc xs0 ps0) (Pick i0) = end ps0 i0 <> MPat dc (toListNP xs0)
  where
    end :: NP (Pattern name) xs -> NS Index xs -> Morphism term
    end Nil       x     = case x of {}
    end (p :* _)  (Z j) = desugarP p j <> MProj1
    end (_ :* ps) (S j) = end ps j <> MProj2

desugarE :: (a -> Morphism term) -> Expression a -> Morphism term
desugarE ctx = go where
    go ExpressionUnit         = MTerminal
    go (ExpressionVar a)      = ctx a
    go (ExpressionTuple x y)  = MProduct (go x) (go y)
    go (ExpressionLeft x)     = MInL <> go x
    go (ExpressionRight y)    = MInR <> go y
    go (ExpressionMany dc es) = MExp dc (map fst es) <> end (map (go . snd) es)

    end :: [Morphism term] -> Morphism term
    end []       = MTerminal
    end (f : fs) = MProduct f (end fs)

-------------------------------------------------------------------------------
-- Generating
-------------------------------------------------------------------------------

generate :: Names -> Morphism (LHsExpr GhcRn) -> LHsExpr GhcRn
generate Names {catNames = CatNames {..}} = go where
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
    go (MPat dc xs)   = hsPar noSrcSpan $ hsApps noSrcSpan (hsVar noSrcSpan catOpfName) [generatePat dc xs]
    go (MExp dc xs)   = hsPar noSrcSpan $ hsApps noSrcSpan (hsVar noSrcSpan catOpfName) [generateExp dc xs]

generatePat :: WrappedName -> [WrappedName] -> LHsExpr GhcRn
generatePat (WrapName dc) (map unwrapName -> xs) = hsLam noSrcSpan
    (dataconPat dc xs)
    (tupleExpr xs)

generateExp :: WrappedName -> [WrappedName] -> LHsExpr GhcRn
generateExp (WrapName dc) (map unwrapName -> xs) = hsLam noSrcSpan
    (tuplePat xs)
    (dataconExpr dc xs)

dataconPat :: GHC.Name -> [GHC.Name] -> LPat GhcRn
dataconPat dc xs = L noSrcSpan $ ConPat
    noExtField
    (L noSrcSpan dc)
    (GHC.PrefixCon [ L noSrcSpan (VarPat noExtField (L noSrcSpan x)) | x <- xs ])

dataconExpr :: GHC.Name -> [GHC.Name] -> LHsExpr GhcRn
dataconExpr dc xs = hsApps noSrcSpan (hsVar noSrcSpan dc) [ hsVar noSrcSpan x | x <- xs ]

tuplePat :: [GHC.Name] -> LPat GhcRn
tuplePat [] = L noSrcSpan $ TuplePat noExtField [] GHC.Boxed
tuplePat (x : xs) = L noSrcSpan $ TuplePat noExtField
    [ L noSrcSpan $ VarPat noExtField (L noSrcSpan x)
    , tuplePat xs
    ]
    GHC.Boxed

tupleExpr :: [GHC.Name] -> LHsExpr GhcRn
tupleExpr []       = hsVar noSrcSpan (GHC.getName (GHC.tupleDataCon GHC.Boxed 0))
tupleExpr (x : xs) = L noSrcSpan $ ExplicitTuple noExtField
    [L noSrcSpan $ Present noExtField z
    | z <- [ hsVar noSrcSpan x , tupleExpr xs ]
    ]
    GHC.Boxed
