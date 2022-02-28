type Symb = Char

data Literal = Lit Symb | Not Literal
  deriving (Show)

infixl 7 :*

infixl 6 :+

infixr 5 :=>

infixr 5 :<=>

data Expr = Var Literal | Expr :* Expr | Expr :+ Expr | Expr :=> Expr | Expr :<=> Expr
  deriving (Show)

a = Var $ Lit 'a'

b = Var $ Lit 'b'

c = Var $ Lit 'c'

d = Var $ Lit 'd'

not' :: Expr -> Expr
not' (Var (Lit a)) = Var (Not $ Lit a)
not' (Var (Not a)) = Var a
not' (a :* b) = not' a :+ not' b
not' (a :+ b) = not' a :* not' b
not' (a :=> b) = a :* not' b
not' (a :<=> b) = (a :* not' b) :+ (not' a :* b)

eliminateImplications :: Expr -> Expr
eliminateImplications (Var a) = Var a
eliminateImplications (a :* b) = eliminateImplications a :* eliminateImplications b
eliminateImplications (a :+ b) = eliminateImplications a :+ eliminateImplications b
eliminateImplications (a :=> b) = not' (eliminateImplications a) :+ eliminateImplications b
eliminateImplications (a :<=> b) =
  let nb = eliminateImplications b
      na = eliminateImplications a
   in (not' na :+ nb) :* (na :+ not' nb)

applyDeMorgansLaws :: Expr -> Expr
applyDeMorgansLaws expr = case expr of (Var (Not (Not (Lit a)))) -> Var (Lit a)
                                       _ -> expr

nnf :: Expr -> Expr
nnf expr = let adl = applyDeMorgansLaws
               ea = eliminateImplications a
               eb = eliminateImplications b
               in case expr of (a :* b)   -> adl $ ea :* eb
                               (a :+ b)   -> adl $ ea :+ eb
                               (a :=> b)  -> adl $ not' a :+ b
                               (a :<=> b) -> adl $ (not' a :+ b) :* (a :+ not' b)
                               _ -> adl expr

distributeConjunctions :: Expr -> Expr
distributeConjunctions (a :* (b :+ c)) = let f = distributeConjunctions in (f a :* f b) :+ (f a :* f c)
distributeConjunctions ((a :+ b) :* c) = let f = distributeConjunctions in (f a :* f c) :+ (f b :* f c)
distributeConjunctions a = a

dnf :: Expr -> Expr
dnf formula = distributeConjunctions $ nnf formula
