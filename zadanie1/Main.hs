import Graph
import System.Environment

main :: IO()

main = do
      [f,s] <- getArgs
      calyPlik <- readFile s
      print calyPlik
