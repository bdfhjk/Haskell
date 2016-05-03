import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as M

type Env = M.Map String Int

type Eval a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a

data Exp = EInt Int
  | Var String
  | Plus Exp Exp
  | Abs String Exp
  | App Exp Exp

eval  :: Exp -> Eval Int

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval (EInt i) = do
  tick
  liftIO $ print i
  return i

eval (Var n) = do
  tick
  tell [n]
  env <- ask
  case M.lookup n env of
    Nothing -> throwError "unbound variable."
    Just val -> return val

eval (Plus e1 e2) = do
  tick
  e1' <- eval e1
  e2' <- eval e2
  return $ e1' + e2'


runEval env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st
