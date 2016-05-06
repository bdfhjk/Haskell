module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Parsyntax
import Lexsyntax
import Abssyntax
import Skelsyntax
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

emptyEnv = M.empty

showTree    :: (Show a, Print a) => Int -> a -> IO ()
putStrV     :: Verbosity -> String -> IO ()
processFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
process     :: Verbosity -> ParseFun Program -> String -> IO ()
main        :: IO ()
bindVal     :: String -> Value -> Env -> Env
evalExp     :: Exp -> Eval Value
evalProgr   :: Program -> Eval Value
run         :: Program -> Verbosity -> IO ()
evalDecl    :: Decl -> Eval ()
evalLambda  :: String -> Exp -> [Exp] -> Eval Value

liftVIntExp   :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Eval Value
liftVBoolExp  :: (Bool -> Bool -> Bool) -> Exp -> Exp -> Eval Value

bindVal    = M.insert
--bindValEnv = foldr (M.insert )

liftVIntExp f e1 e2 = do
  (VInt e1') <- evalExp e1
  (VInt e2') <- evalExp e2
  return $ VInt $ f e1' e2'

liftVBoolExp f e1 e2 = do
  (VBool e1') <- evalExp e1
  (VBool e2') <- evalExp e2
  return $ VBool $ f e1' e2'

evalLambda s e l =
  case l of
    []  -> error "Need more arguments"
    [h] -> do
      e' <- evalExp h
      local (bindVal s e') (evalExp e)
    h:t  -> do
      e' <- evalExp h
      local (bindVal s e') (evalExp (ECall2 e t))


evalExp (EInt v)     = return (VInt v)
evalExp (EAdd e1 e2) = liftVIntExp (+) e1 e2
evalExp (ESub e1 e2) = liftVIntExp (-) e1 e2
evalExp (EMul e1 e2) = liftVIntExp (*) e1 e2
evalExp (EDiv e1 e2) = liftVIntExp quot e1 e2
evalExp (EComp e1 e2)= liftVBoolExp (<) e1 e2
evalExp (ELambda (Ident s) e) = return (VLambda s e)
evalExp (ECall2 e l) = do
  e' <- evalExp e
  case e' of
    VLambda s e2 -> evalLambda s e2 l
    _ -> error "Incorrect function call2"

evalExp (ECall (Ident f) l) = do
  m <- get
  case M.lookup f m of
    Just (VClos env e) -> case e of
      ELambda (Ident s) e' -> evalLambda s e' l
      _ -> local (const env) (evalExp e)
    Just (VLambda s e) -> evalLambda s e l
    _ -> error "Incorrect function call"

evalExp (ELet (Bind (Ident s) e') e) = do
    e'' <- evalExp e'
    local (bindVal s e'') (evalExp e)

evalExp (EVar (Ident s)) = do
  m <- ask
  case M.lookup s m of
    Just a -> return a
    _ -> error "Incorrect variable"

showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

putStrV v s = when (v > 1) $ putStrLn s

processFile v p f = readFile f >>= process v p

evalDecl (DType _ _) = return ()
evalDecl (DFun (Ident n) l e) = do
  m  <- get
  case l of
    []  -> put (M.insert n (VClos emptyEnv e) m)
    _   -> let e' = foldr ELambda e l in
           put (M.insert n (VClos emptyEnv e') m)

evalDecls = mapM_ evalDecl

evalProgr (Progr f e) = do
  evalDecls f
  evalExp e

run p v = do
  result <- runWriterT
           (runExceptT
           (runStateT
           (runReaderT (evalProgr p) emptyEnv) emptyEnv))
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
