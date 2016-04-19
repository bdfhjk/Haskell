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

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree


type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
run     :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()

putStrV v s = if v > 1 then putStrLn s else return ()
runFile v p f = readFile f >>= run v p

run v p s = let ts = myLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree



main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 0 pExp
            "-v":fs -> case fs of 
                fss:[] -> runFile 2 pExp fss
                fss -> putStrLn "Incorrect arguments"
            fs:[] -> runFile 0 pExp fs
            fs -> putStrLn "Incorrect arguments"

