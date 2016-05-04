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
type Env = M.Map String Int
type Eval a = ReaderT Env (ExceptT String (WriterT [String] IO)) a

envLookup s   env = env s
envBind   s v env = (\s' -> if s == s' then v else env s)
emptyEnv = error "Not found"

showTree    :: (Show a, Print a) => Int -> a -> IO ()
putStrV     :: Verbosity -> String -> IO ()
processFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
process     :: Verbosity -> ParseFun Program -> String -> IO ()
main        :: IO ()

evalExp   :: Exp -> Eval Integer
evalProgr :: Program -> Eval Integer
run       :: Program -> Verbosity -> IO()

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

--evalProgr (Progr d e) = evalExp e
--run e  = evalProgr e emptyEnv

--runEval e =  runWriterT (runErrorT (runReaderT evalProgr e emptyEnv))

showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

putStrV v s = when (v > 1) $ putStrLn s

processFile v p f = readFile f >>= process v p

evalProgr (Progr _ e) = evalExp e
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
