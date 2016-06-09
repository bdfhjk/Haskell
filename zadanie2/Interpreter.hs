module Main where

import System.Environment ( getArgs )
import Parsyntax
import Abssyntax
import Printsyntax
import ErrM
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.State
import qualified Data.Map as M
import Evaluator

showTree    :: (Show a, Print a) => Int -> a -> IO ()
putStrV     :: Verbosity -> String -> IO ()
processFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
process     :: Verbosity -> ParseFun Program -> String -> IO ()
main        :: IO ()
evalProgr   :: Program -> Eval Value
run         :: Program -> Verbosity -> IO ()

showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

putStrV v s = when (v > 1) $ putStrLn s

processFile v p f = readFile f >>= process v p

evalProgr (Progr f e) = do
  evalDecls f
  evalExp e

run p v = do
  result <- runExceptT
           (runStateT
           (runReaderT (evalProgr p) M.empty) M.empty)
  case result of
    Left e -> print e
    Right (VInt i, _) -> print i
    _ -> if v>1
        then putStrV v $ show result
        else putStrLn "Error: Evaluation not finished due to insufficient arguments"

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
