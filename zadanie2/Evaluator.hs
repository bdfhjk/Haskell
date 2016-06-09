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
evalBExp    :: BExp -> Eval Value
evalDecl    :: Decl -> Eval ()
evalDecls   :: [Decl] -> Eval ()
evalCall    :: Value -> [Exp] -> Eval Value

-- Helper functions to evaluate simple arithmetic / comparison operations
liftVIntExp :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Eval Value
liftVBoolExp:: (Bool -> Bool -> Bool) -> BExp -> BExp -> Eval Value

-- Declared / real type checking
matchTV     :: Type -> Value -> Eval ()

-- Binding value in the local enviroment
bindVal     :: String -> Value -> Env -> Env

bindVal    = M.insert

liftVIntExp f e1 e2 = do
  (VInt e1') <- evalExp e1
  (VInt e2') <- evalExp e2
  return $ VInt $ f e1' e2'

liftVBoolExp f e1 e2 = do
  (VBool e1') <- evalBExp e1
  (VBool e2') <- evalBExp e2
  return $ VBool $ f e1' e2'

-- Convert list of function arguments into recursive lambda expression.
evalDeclWrapLambda (TDef _ (Ident h):t) e =
  VLambda h (evalDeclWrapLambda t e)

evalDeclWrapLambda [] e = VExp e

evalDecl (DFun (TDef _ (Ident s)) l e) = do
  m  <- get
  put (M.insert s (VClos M.empty (evalDeclWrapLambda l e)) m)

evalDecls = mapM_ evalDecl

evalBExp (CTrue)        = return (VBool True)
evalBExp (CFalse)       = return (VBool False)
evalBExp (BComp b1 b2)  = liftVBoolExp (==) b1 b2
evalBExp (EComp e1 e2)  = do
  (VInt e1') <- evalExp e1
  (VInt e2') <- evalExp e2
  return $ VBool $ e1' < e2'

evalBExp (EComp2 e1 e2) = do
  (VInt e1') <- evalExp e1
  (VInt e2') <- evalExp e2
  return $ VBool $ e1' == e2'

evalBExp (BEmpty e) = do
  e' <- evalExp e
  case e' of
    (VCons 0 _ ) -> return (VBool True)
    _ -> return (VBool False)

evalExpWrapLambda e =
  case e of
    ELambda (TDef _ (Ident s)) e' -> do
      e'' <- evalExpWrapLambda e'
      return (VLambda s e'')
    _ -> return (VExp e)

evalExp (EIf b e1 e2)= do
  (VBool b') <- evalBExp b
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
evalExp (EAdd e1 e2) = liftVIntExp (+) e1 e2
evalExp (ESub e1 e2) = liftVIntExp (-) e1 e2
evalExp (EMul e1 e2) = liftVIntExp (*) e1 e2
evalExp (EDiv e1 e2) = liftVIntExp quot e1 e2

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
