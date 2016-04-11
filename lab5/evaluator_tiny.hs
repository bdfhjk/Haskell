import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Functor

type Var = String
data Exp = EInt Int
   | EOp  Op Exp Exp
   | EVar Var
   | ELet Var Exp Exp  -- let var = e1 in e2

data Stmt = Exp
   | SSkip
   | SAssign Var Exp
   | SConcat Stmt Stmt
   | SIf Exp Stmt Stmt
   | SWhile Exp Stmt

data Op  = OpAdd | OpMul | OpSub
type Env = Map Var Int
type R a  = Env -> a
type S a  = State Env a

evalExp :: Exp -> R Int
evalOp :: Op -> Int -> Int -> Int
execStmt :: Stmt -> IO()
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

execStmt stmt = do
  let sk = execState (exec stmt) Map.empty
  print sk

exec :: Stmt -> S ()

exec SSkip = return ()
exec (SAssign var expr) = do
  -- env <- get
  -- let val = evalExpr expr env
  ---------
  -- val <- get >>= return . evalExp expr
  ---------
  val <- evalExp expr <$> get

  modify (Map.insert var val)

exec (SConcat stmt1 stmt2) = do
  exec stmt1
  exec stmt2

exec (SIf expr stmt1 stmt2) = do
  env <- get
  let be = evalExp expr env
  if be /= 0 then
    exec stmt1
  else
    exec stmt2

exec (SWhile expr stmt) = do
  env <- get
  let evl = evalExp expr env
  if evl == 0 then
    return ()--SSkip
  else do
    exec stmt
    exec (SWhile expr stmt)


--test :: Exp
--test = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
--                      (EOp OpSub y (EInt 1)))
--                (EOp OpMul x (EInt 3))
--    where x = EVar "x"
--          y = EVar "y"

test2 :: Stmt
test2 = SIf 0 (SAssign "aa" 5) (SAssign "bb" 4)
main = execStmt test2
