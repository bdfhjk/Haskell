module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Parsyntax
import Lexsyntax
import Abssyntax
import Skelsyntax
import Printsyntax
import ErrM

type ParseFun a = [Token] -> Err a
type Verbosity = Int
type Env = String -> Exp

envLookup s   env = env s
envBind   s v env = (\s' -> if s == s' then v else env s) 
emptyEnv = error "Not found"

showTree    :: (Show a, Print a) => Int -> a -> IO ()
putStrV     :: Verbosity -> String -> IO ()
processFile :: Verbosity -> ParseFun Exp -> FilePath -> IO ()
process     :: Verbosity -> ParseFun Exp -> String -> IO ()
main        :: IO ()

eval :: Exp -> Env -> Integer
run  :: Exp -> Integer

eval (EInt v) _ = v
eval (EAdd e1 e2) env = (eval e1 env) + (eval e2 env)
eval (ESub e1 e2) env = (eval e1 env) - (eval e2 env)
eval (EMul e1 e2) env = (eval e1 env) * (eval e2 env)
eval (EDiv e1 e2) env = quot (eval e1 env) (eval e2 env)

run e  = eval e emptyEnv


showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

putStrV v s = if v > 1 then putStrLn s else return ()

processFile v p f = readFile f >>= process v p

process v p s = let ts = myLexer s in case p ts of
           Bad s    -> do putStrLn "Parse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
           Ok  tree -> do putStrV v "Parse Successful!"
                          showTree v tree
                          putStrLn $ show (run tree)



main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= process 0 pExp
            "-v":fs -> case fs of 
                fss:[] -> processFile 2 pExp fss
                fss    -> putStrLn "Incorrect arguments"
            fs:[]      -> processFile 0 pExp fs
            fs         -> putStrLn "Incorrect arguments"

