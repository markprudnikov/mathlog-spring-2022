module Main where
main :: IO ()
main = return ()

type Symb = Char


data Literal = Lit Symb | NegLit Symb

instance Show Literal where
    show (Lit s) = [s]
    show (NegLit s) = "!" ++ [s]

infix 7 :*

infix 6 :+

infixr 5 :=>

infixr 5 :<=>

data Expr = Var Literal | Expr :* Expr | Expr :+ Expr | Expr :=> Expr | Expr :<=> Expr | Not Expr

instance Show Expr where
    show (Var lit) = show lit
    show (e1 :* e2) = show e1 ++ " /\\ " ++ show e2
    show (e1 :+ e2) = "(" ++ show e1 ++ " \\/ " ++ show e2 ++ ")"
    show (Not expr) = case expr of (e1 :+ e2) -> "!" ++ show expr
                                   _ -> "!(" ++ show expr ++ ")"
    show (e1 :=> e2) = "(" ++ show e1 ++ " :=> " ++ show e2 ++ ")"
    show (e1 :<=> e2) = "(" ++ show e1 ++ " :=> " ++ show e2 ++ ")"

x = Var (Lit 'x')
y = Var (Lit 'y')
z = Var (Lit 'z')
w = Var (Lit 'w')

nx = Var (NegLit 'x')
ny = Var (NegLit 'y')
nw = Var (NegLit 'w')
nz = Var (NegLit 'z')

not' :: Expr -> Expr
not' (Var (Lit a)) = Var $ NegLit a
not' (Var (NegLit a)) = Var $ Lit a
not' (Not a) = a
not' (a :* b) = not' a :+ not' b
not' (a :+ b) = not' a :* not' b
not' (a :=> b) = a :* not' b
not' (a :<=> b) = (a :* not' b) :+ (not' a :* b)

eliminateImplications :: Expr -> Expr
eliminateImplications (Var a) = Var a
eliminateImplications (Not a) = eliminateImplications a
eliminateImplications (a :* b) = eliminateImplications a :* eliminateImplications b
eliminateImplications (a :+ b) = eliminateImplications a :+ eliminateImplications b
eliminateImplications (a :=> b) = not' (eliminateImplications a) :+ eliminateImplications b
eliminateImplications (a :<=> b) =
  let nb = eliminateImplications b
      na = eliminateImplications a
   in (not' na :+ nb) :* (na :+ not' nb)

nnf :: Expr -> Expr
nnf expr = let adl = pushNegation
               eI = eliminateImplications
               in case expr of (a :* b)   -> adl $ eI a :* eI b
                               (a :+ b)   -> adl $ eI a :+ eI b
                               (a :=> b)  -> adl $ not' a :+ b
                               (a :<=> b) -> adl $ (not' a :+ b) :* (a :+ not' b)
                               (Not a)    -> adl $ not' $ eI a
                               _ -> adl expr


pushNegation :: Expr -> Expr
pushNegation (Not (Var (NegLit s))) = Var $ Lit s
pushNegation (Not (Var (Lit s))) = Var $ NegLit s
pushNegation (Not (Not ex)) = ex
pushNegation (Not (e1 :* e2)) = not' e1 :+ not' e2
pushNegation (Not (e1 :+ e2)) = not' e1 :* not' e2
pushNegation (Not (e1 :=> e2)) = e1 :* not' e2
pushNegation (Not (e1 :<=> e2)) = (e1 :* not' e2) :+ (not' e1 :* e2)
pushNegation expr = expr

distributeConjunctions :: Expr -> Expr
distributeConjunctions expr =
    let f = distributeConjunctions in
    case expr of
        (a :* (b :+ c)) -> (f a :* f b) :+ (f a :* f c)
        ((a :+ b) :* c) -> (f a :* f c) :+ (f b :* f c)
        (a :* b) -> f a :* f b
        (a :+ b) -> f a :+ f b
        (Not a) -> Not (f a)
        _ -> expr 

dnf :: Expr -> Expr
dnf formula = distributeConjunctions $ nnf formula

distributeDisjunctions :: Expr -> Expr
distributeDisjunctions expr =
    let d = distributeDisjunctions in
    case expr of
        (a :+ (b :* c)) -> (d a :+ d b) :* (d a :+ d c)
        ((a :* b) :+ c) -> (d a :+ d b) :* (d b :+ d c)
        (a :* b) -> d a :* d b
        (a :+ b) -> d a :+ d b
        (Not a) -> Not (d a)
        _ -> expr

cnf :: Expr -> Expr
cnf formula = distributeDisjunctions $ nnf formula

