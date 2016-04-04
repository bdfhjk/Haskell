import Control.Monad.Reader

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

type R a = Int -> a
renumber :: Tree a -> R(Tree Int)

renumber Empty = return Empty
renumber (Node _ tl tr) = do
  d <- ask
  tl' <- local (+1) (renumber tl)
  tr' <- local (+1) (renumber tr)
  return (Node d tl' tr')

type Var = String
data Exp = EInt Int
   | EOp  Op Exp Exp
   | EVar Var
   | ELet Var Exp Exp  -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub

evalExp :: Exp -> Int
evalExp (EInt a) = a
evalExp (EOp op exp1 exp2) = op (evalExp exp1) (evalExp exp2)

  {--
  data Env = Env {var_a :: Int, var_b :: Int}
    deriving Show
  type R a = Env -> a

  startEnv = Env {var_a = 5, var_b = 10}

  example :: R Int
  example = do
    a <- asks var_a
    b <- asks var_b
    return $ a+b

  example2 :: R Int
  example2 = liftM2 (+) (asks var_a) (asks var_b)

  run :: R a -> a
  run r = r startEnv
  --}
