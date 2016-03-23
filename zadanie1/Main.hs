import Graph
import System.Environment
import System.IO

main :: IO()

rInt :: String -> Int
rInt = read

analyze :: String -> [Int]
minim :: String -> Int
maxim :: String -> Int

minim c = minimum (map rInt (words c))
maxim c = maximum (map rInt (words c))

processNeighbors :: Graph -> Int -> [Int] -> Graph
processNeighbors g _ [] = g
processNeighbors g w1 (h:tl) = processNeighbors (addNeighbor g w1 h) w1 tl

processLine :: Graph -> String -> Graph
processLine g c = processNeighbors g w1 w2
  where
    w2 = tail r
    w1 = head r
    r  = map rInt w
    w  = words c

analyze c = getRoute ge lo hi 1
  where
    ge = foldl processLine g (lines c)
    g = createGraph (lo, hi)
    lo = minim c
    hi = maxim c

main = do
        args <- getArgs
        if null args
        then do
           contents <- getContents
           print (analyze contents)
        else do
           handle <- openFile (head args) ReadMode
           contents <- hGetContents handle
           hClose handle
           if null (words contents)
           then putStrLn "Error: Empty file."
           else print (analyze contents)
