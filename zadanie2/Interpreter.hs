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
type Env = M.Map String Integer
type Eval a = ReaderT Env (ExceptT String (WriterT [String] IO)) a

emptyEnv = M.empty

showTree    :: (Show a, Print a) => Int -> a -> IO ()
putStrV     :: Verbosity -> String -> IO ()
processFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
process     :: Verbosity -> ParseFun Program -> String -> IO ()
main        :: IO ()
bindVal        :: String -> Integer -> Env -> Env

evalExp   :: Exp -> Eval Integer
evalProgr :: Program -> Eval Integer
run       :: Program -> Verbosity -> IO()

bindVal = M.insert

evalExp (EInt v) = return v

evalExp (EAdd e1 e2) = do
  e1' <- evalExp e1
  e2' <- evalExp e2
  return $ e1' + e2'

evalExp (ESub e1 e2) = do
  e1' <- evalExp e1
  e2' <- evalExp e2
  return $ e1' - e2'

evalExp (EMul e1 e2) = do
  e1' <- evalExp e1
  e2' <- evalExp e2
  return $ e1' * e2'

evalExp (EDiv e1 e2) = do
  e1' <- evalExp e1
  e2' <- evalExp e2
  return $ quot e1' e2'

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

evalProgr (Progr _ e) = do
  evalExp e

run p v = do
  result <- runWriterT (runExceptT (runReaderT (evalProgr p) emptyEnv))
  mapM_ (putStrV v) (snd result)
  case fst result of
    Left e -> print e
    Right i -> print i

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
