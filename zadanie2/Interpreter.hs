module Main where

import System.Environment ( getArgs )
import Parsyntax
import Lexsyntax
import Abssyntax
import Printsyntax
import ErrM
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as M

type ParseFun a = [Token] -> Err a
type Verbosity = Int
type Env = M.Map String Value

-- Monad reader -> dynamic state of execution
-- Monad state  -> static environment
-- Monad except -> reporting runtime errors
-- Monad writer -> interpreter-debugging logs
type Eval a = ReaderT Env (StateT Env (ExceptT String (WriterT [String] IO))) a

data Value = VInt Integer           |
             VBool Bool             |
             VClos Env Exp          |
             VCons Int Value        |
             VPair Value Value      |
             VLambda String Exp deriving(Show)


showTree    :: (Show a, Print a) => Int -> a -> IO ()
putStrV     :: Verbosity -> String -> IO ()
processFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
process     :: Verbosity -> ParseFun Program -> String -> IO ()
main        :: IO ()
bindVal     :: String -> Value -> Env -> Env
evalExp     :: Exp -> Eval Value
evalBExp    :: BExp -> Eval Value
evalProgr   :: Program -> Eval Value
evalDecl    :: Decl -> Eval ()
evalDecls   :: [Decl] -> Eval ()
evalLambda  :: String -> Exp -> [Exp] -> Eval Value
evalCall    :: Value -> [Exp] -> Eval Value
run         :: Program -> Verbosity -> IO ()
liftVIntExp   :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Eval Value
liftVBoolExp  :: (Bool -> Bool -> Bool) -> BExp -> BExp -> Eval Value

bindVal    = M.insert

liftVIntExp f e1 e2 = do
  (VInt e1') <- evalExp e1
  (VInt e2') <- evalExp e2
  return $ VInt $ f e1' e2'

liftVBoolExp f e1 e2 = do
  (VBool e1') <- evalBExp e1
  (VBool e2') <- evalBExp e2
  return $ VBool $ f e1' e2'

evalLambda s e l =
  case l of
    []  -> error "Need more arguments"
    [h] -> do
      e' <- evalExp h
      local (bindVal s e') (evalExp e)
    h:t  -> do
      e' <- evalExp h
      local (bindVal s e') (evalExp (ECall e t))

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
    (VCons 0 (VInt 0)) -> return (VBool True)
    _ -> return (VBool False)

evalExp (EIf b e1 e2)= do
  (VBool b') <- evalBExp b
  if b' then evalExp e1 else evalExp e2

evalExp (CList)         = return (VCons 0 (VInt 0))
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

evalExp (ELambda (Ident s) e) = return (VLambda s e)

evalExp (ECall e l) = case e of
    EVar (Ident f) -> do
      m <- get
      case M.lookup f m of
        Just v -> evalCall v l
        Nothing -> error "Incorrect function call"
    _ -> do
      e' <- evalExp e
      evalCall e' l

evalExp (ELet (Bind (Ident s) e') e) = do
    e'' <- evalExp e'
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


showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

putStrV v s = when (v > 1) $ putStrLn s

processFile v p f = readFile f >>= process v p

evalDecl (DFun (Ident n) l e) = do
  m  <- get
  case l of
    []  -> put (M.insert n (VClos M.empty e) m)
    _   -> let e' = foldr ELambda e l in
           put (M.insert n (VClos M.empty e') m)

evalDecls = mapM_ evalDecl

evalProgr (Progr f e) = do
  evalDecls f
  evalExp e

run p v = do
  result <- runWriterT
           (runExceptT
           (runStateT
           (runReaderT (evalProgr p) M.empty) M.empty))
  mapM_ (putStrV v) (snd result)
  case fst result of
    Left e -> print e
    Right (VInt i, _) -> print i
    _ -> print result

process v p s = let ts = myLexer s in case p ts of
           Bad e    -> do putStrLn "Parse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn e
           Ok  tree -> do putStrV v "Parse Successful!"
                          showTree v tree
                          run tree v



main = do args <- getArgs
          case args of
            [] -> getContents >>= process 0 pProgram
            "-v":fs -> case fs of
                [fss] -> processFile 2 pProgram fss
                _   -> putStrLn "Incorrect arguments"
            [fs]    -> processFile 0 pProgram fs
            _      -> putStrLn "Incorrect arguments"
