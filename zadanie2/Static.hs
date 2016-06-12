module Static where
import Abssyntax
import Data.Map as M
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.State

-- Environment store local variables' types
type EnvT = M.Map String Type

-- Monad reader -> dynamic state of execution - global definitions of types
-- Monad state  -> static environment - local variables' types
-- Monad except -> reporting static error
type SEval a = ReaderT EnvT (StateT EnvT (ExceptT String IO)) a

-- Static check of expressions and declarations
checkExp              :: Exp -> SEval Type
checkDecls            :: [Decl] -> SEval ()
checkDecl             :: Decl -> SEval ()
checkDeclWrapLambda   :: [TDf] -> Exp -> SEval Type
checkArithmetic       :: Exp -> Exp -> SEval Type
checkLogic            :: Exp -> Exp -> SEval Type
checkList             :: Exp -> SEval Type
checkCall             :: Type -> [Exp] -> SEval Type
checkExpWrapLambda    :: Exp -> SEval Type


checkArithmetic e1 e2 = do
  t1' <- checkExp e1
  t2' <- checkExp e2
  case t1' of
    TInt -> case t2' of
      TInt -> return TInt
      _ -> error "Static check failed: Expected integer in arithmetic"
    _ -> error "Static check failed: Expected integer in arithmetic"

checkLogic e1 e2 = do
  t1' <- checkExp e1
  t2' <- checkExp e2
  case t1' of
    TInt -> case t2' of
      TInt -> return TBool
      _ -> error "Static check failed: Comparison incompatibility"
    TBool -> case t2' of
      TBool -> return TBool
      _ -> error "Static check failed: Comparison incompatibility"
    _ -> error "Static check failed: Comparison incompatibility"

checkList e = do
  t <- checkExp e
  case t of
    (TList t') -> return t'
    _ -> error "Static check failed: Head used on a non-list object."

-- Convert list of function arguments into recursive lambda expression.
checkDeclWrapLambda [] e = checkExp e

checkDeclWrapLambda (TDef t (Ident s):tl) e = do
  t' <- local (M.insert s t) (checkDeclWrapLambda tl e)
  return (TFun t t')

checkDecl (DFun (TDef t (Ident s)) l e) = do
  t' <- local (M.insert s t) (checkDeclWrapLambda l e)
  m  <- get
  if t /= t'
    then error "Static check failed: Invalid function definition."
    else put (M.insert s t' m)

checkDecls                = mapM_ checkDecl

checkExp (EInt _)         = return TInt

checkExp (EAdd e1 e2)     = checkArithmetic e1 e2

checkExp (ESub e1 e2)     = checkArithmetic e1 e2

checkExp (EMul e1 e2)     = checkArithmetic e1 e2

checkExp (EDiv e1 e2)     = checkArithmetic e1 e2

checkExp (CTrue)          = return TBool

checkExp (CFalse)         = return TBool

checkExp (ECompE e1 e2)   = checkLogic e1 e2

checkExp (ECompL e1 e2)   = checkLogic e1 e2

checkExp (EHead e)        = checkList e

checkExp (ETail e)        = checkExp e

checkExp (EEmpty e) = do
  t <- checkExp e
  case t of
    TList _ -> return TBool
    _ -> error "Static check failed: Empty used on a non-list object"

checkExp (CList t)        = return (TList t)

checkExp (EAppend e1 e2)  = do
  t1 <- checkExp e1
  t2 <- checkExp e2
  case t2 of
    TList t3 -> if t1 == t3
                then return (TList t1)
                else error "Static check failed: Append type problem."
    _ -> error "Static check failed: Append type problem."

checkExp (EIf e1 e2 e3)   = do
    t1 <- checkExp e1
    t2 <- checkExp e2
    t3 <- checkExp e3
    if t1 /= TBool
      then error "Static check failed: If condition is not Bool"
    else if t2 /= t3
      then error "Static check failed: If then else types mistmatch"
    else return t2

checkExp (EVar (Ident s)) = do
  m  <- ask
  m2 <- get
  case M.lookup s m of
    Just a -> return a
    Nothing -> case M.lookup s m2 of
      Just a -> return a
      Nothing -> error ("Static check failed: Incorrect variable or function " ++ show s)

checkExp (ELet (TDef t (Ident s)) e' e) = do
    t' <- checkExp e'
    if t /= t'
      then error "Static check failed: Let type mistmatch"
      else local (M.insert s t') (checkExp e)

checkExp (ECall e l) = do
  t' <- checkExp e
  checkCall t' l

checkExp el@(ELambda _ _) =
  -- Recursively convert ELambdas into TFuns
  checkExpWrapLambda el

checkCall t [] = return t

checkCall (TFun t1 t2) (h:tl) = do
  t' <- checkExp h
  if t1 /= t'
    then error "Static check failed: Application type mistmatch"
    else checkCall t2 tl

checkCall _ _ = error "Static check failed: Incorrect call usage"

checkExpWrapLambda e =
  case e of
    ELambda (TDef t (Ident s)) e' -> do
      t' <- local (M.insert s t) (checkExpWrapLambda e')
      return (TFun t t')
    _ -> checkExp e
