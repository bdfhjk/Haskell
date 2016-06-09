module Evaluator where
import Lexsyntax
import Abssyntax
import ErrM
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.State
import qualified Data.Map as M


type ParseFun a = [Token] -> Err a
type Verbosity = Int

-- Environment store local variables
type Env = M.Map String Value

-- Monad reader -> dynamic state of execution - global definitions
-- Monad state  -> static environment - local variables
-- Monad except -> reporting runtime errors
-- Monad writer -> interpreter/debugging logs
type Eval a = ReaderT Env (StateT Env (ExceptT String IO)) a

data Value = VInt Integer           |
             VBool Bool             |
             VClos Env Value        |
             VCons Int Type         |
             VPair Value Value      |
             VLambda String Value   |
             VExp Exp                       deriving(Show)

evalExp     :: Exp -> Eval Value
evalDecl    :: Decl -> Eval ()
evalDecls   :: [Decl] -> Eval ()
evalCall    :: Value -> [Exp] -> Eval Value

-- Helper functions to evaluate simple arithmetic / comparison operations
--liftExp :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Eval Value

-- Declared / real type checking
matchTV     :: Type -> Value -> Eval ()

-- Binding value in the local enviroment
bindVal     :: String -> Value -> Env -> Env

bindVal    = M.insert


liftExp f e1 e2 = do
  e1' <- evalExp e1
  e2' <- evalExp e2
  case e1' of
    (VInt i1) -> case e2' of
      (VInt i2) -> return $ VInt $ f i1 i2
      _ -> error "Arithmetic error."
    _ -> error "Arithmetic error."


-- Convert list of function arguments into recursive lambda expression.
evalDeclWrapLambda (TDef _ (Ident h):t) e =
  VLambda h (evalDeclWrapLambda t e)

evalDeclWrapLambda [] e = VExp e

evalDecl (DFun (TDef _ (Ident s)) l e) = do
  m  <- get
  put (M.insert s (VClos M.empty (evalDeclWrapLambda l e)) m)

evalDecls = mapM_ evalDecl

evalExpWrapLambda e =
  case e of
    ELambda (TDef _ (Ident s)) e' -> do
      e'' <- evalExpWrapLambda e'
      return (VLambda s e'')
    _ -> return (VExp e)

evalExp (CTrue)         = return (VBool True)
evalExp (CFalse)        = return (VBool False)

evalExp (ECompE e1 e2)  = do
  e1' <- evalExp e1
  e2' <- evalExp e2
  case e1' of
    (VInt i1) -> case e2' of
      (VInt i2) -> return $ VBool $ i1 == i2
      _ -> error "Arithmetic error."
    (VBool b1) -> case e2' of
      (VBool b2) -> return $ VBool $ b1 == b2
      _ -> error "Arithmetic error."
    _ -> error "Arithmetic error."

evalExp (ECompL e1 e2)  = do
  e1' <- evalExp e1
  e2' <- evalExp e2
  case e1' of
    (VInt i1) -> case e2' of
      (VInt i2) -> return $ VBool $ i1 < i2
      _ -> error "Arithmetic error."
    (VBool b1) -> case e2' of
      (VBool b2) -> return $ VBool $ b1 < b2
      _ -> error "Arithmetic error."
    _ -> error "Arithmetic error."

evalExp (EEmpty e) = do
  e' <- evalExp e
  case e' of
    (VCons 0 _ ) -> return (VBool True)
    _ -> return (VBool False)

evalExp (EIf b e1 e2)= do
  (VBool b') <- evalExp b
  if b' then evalExp e1 else evalExp e2

evalExp (CList t) = return (VCons 0 t)

evalExp (EAppend e1 e2) = do
  e1' <- evalExp e1
  e2' <- evalExp e2
  return (VPair e1' e2')

evalExp (EHead e) = do
  e' <- evalExp e
  case e' of
    (VPair a _) -> return a
    _ -> error "Head used on a non-list object"

evalExp (ETail e) = do
  e' <- evalExp e
  case e' of
    (VPair _ a) -> return a
    _ -> error "Tail used on a non-list object"

evalExp (EInt v)     = return (VInt v)
evalExp (EAdd e1 e2) = liftExp (+) e1 e2
evalExp (ESub e1 e2) = liftExp (-) e1 e2
evalExp (EMul e1 e2) = liftExp (*) e1 e2
evalExp (EDiv e1 e2) = liftExp quot e1 e2

evalExp el@(ELambda _ _) = do
  -- Recursively convert ELambdas into VLambdas
  e' <- evalExpWrapLambda el

  return (VClos M.empty e')

evalExp (ECall e l) = case e of
    EVar (Ident f) -> do
      m <- get
      case M.lookup f m of
        Just v -> evalCall v l
        Nothing -> error "Incorrect function call"
    _ -> do
      e' <- evalExp e
      evalCall e' l

evalExp (ELet (TDef t (Ident s)) e' e) = do
    e'' <- evalExp e'
    matchTV t e''
    local (bindVal s e'') (evalExp e)

evalExp (EVar (Ident s)) = do
  m  <- ask
  m2 <- get
  case M.lookup s m of
    Just a -> return a
    Nothing -> case M.lookup s m2 of
      Just a -> return a
      Nothing -> error ("Incorrect variable or function " ++ show s)

-- Procedural call
evalCall (VClos env (VExp e)) [] = local (const env) (evalExp e)

-- Applicative call
evalCall (VClos env (VLambda s e)) [] = return (VClos env (VLambda s e))
evalCall (VClos env (VLambda s e)) (h:t) = do
  h' <- evalExp h
  evalCall (VClos (M.insert s h' env) e) t

evalCall _ _ = error "Call on an incorrect object"

matchTV t v =
  case t of
    TInt -> case v of
      VInt _ -> return ()
      _ -> error "Static type check failed."
    TBool -> case v of
      VBool _ -> return ()
      _ -> error "Static type check failed."
    TList _ -> return ()
    TFun _ _ -> return ()
