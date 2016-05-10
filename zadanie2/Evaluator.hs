module Evaluator where
import Lexsyntax
import Abssyntax
import ErrM
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Writer
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
type Eval a = ReaderT Env (StateT Env (ExceptT String (WriterT [String] IO))) a

data Value = VInt Integer           |
             VBool Bool             |
             VClos Env Exp          |
             VCons Int Type         |
             VPair Value Value      |
             VLambda String Exp deriving(Show)

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

evalDecl (DFun (TDef _ (Ident n)) l e) = do
  m  <- get
  case l of
    []  -> put (M.insert n (VClos M.empty e) m)
    _   -> let e' = foldr ELambda e l in
           put (M.insert n (VClos M.empty e') m)

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

evalExp (EIf b e1 e2)= do
  (VBool b') <- evalBExp b
  if b' then evalExp e1 else evalExp e2

evalExp (CListInt)         = return (VCons 0 TInt)
evalExp (CListBool)        = return (VCons 0 TBool)
evalExp (CListList)        = return (VCons 0 TList)
evalExp (CListFun)         = return (VCons 0 TFun)

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

evalExp (ELambda (TDef _ (Ident s)) e) = return (VLambda s e)

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

evalCall vc@(VClos env e) [] = case e of
  ELambda _ _ -> return vc
  _ -> local (const env) (evalExp e)

evalCall (VClos env e) (h:t) = case e of
  el@(ELambda _ _) -> do
    (VLambda s e') <- evalExp el
    h' <- evalExp h
    let env' = M.insert s h' env
    evalCall (VClos env' e') t
  _ -> local (const env) (evalExp e)

evalCall vl@(VLambda _ _) [] = return vl
evalCall (VLambda s e) [h] = do
  h' <- evalExp h
  local (bindVal s h') (evalExp e)
evalCall (VLambda s e) (h:t) = do
  h' <- evalExp h
  case e of
    el@(ELambda _ _) -> do
      el' <- evalExp el
      local (bindVal s h') (evalCall el' t)
    ex -> evalExp ex

evalCall _ _ = error "Call on an incorrect object"

matchTV t v =
  case t of
    TInt -> case v of
      VInt _ -> return ()
      _ -> error "Static type check failed."
    TBool -> case v of
      VBool _ -> return ()
      _ -> error "Static type check failed."
    TFun -> return ()
    TList -> return ()
