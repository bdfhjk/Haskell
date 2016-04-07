import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map (Map)

type Var = String
data Exp = EInt Int
   | EOp  Op Exp Exp
   | EVar Var
   | ELet Var Exp Exp  -- let var = e1 in e2

data Op  = OpAdd | OpMul | OpSub
type Env = Map Var Int
type R a = Env -> a

evalExp :: Exp -> R Int
evalOp :: Op -> Int -> Int -> Int
main :: IO()

instance Num Exp where
  fromInteger = EInt . fromInteger
  (*) = EOp OpMul
  (-) = EOp OpSub
  (+) = EOp OpAdd
  abs = undefined
  signum = undefined


evalExp (EInt a) = return a
evalExp (EOp op exp1 exp2) = do
   v1 <- evalExp exp1
   v2 <- evalExp exp2
   return (evalOp op v1 v2)

evalExp (EVar var) = do
  env <- ask
  case Map.lookup var env of
    Just a -> return a
    Nothing -> error "Variable not found"

evalExp (ELet var e1 e2) = do
  ev1 <- evalExp e1
  local (Map.insert var ev1) (evalExp e2)

evalOp OpAdd = (+)
evalOp OpMul = (*)
evalOp OpSub = (-)

test :: Exp
test = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
                      (EOp OpSub y (EInt 1)))
                (EOp OpMul x (EInt 3))
    where x = EVar "x"
          y = EVar "y"
main = print $ evalExp test Map.empty
