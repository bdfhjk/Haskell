import Graph
import System.Environment
import System.IO

main             :: IO()

-- | Convert String to Int.
rInt             :: String -> Int

-- | Process a file's singe line, expanding graph.
processLine      :: Graph -> String -> Graph

-- | Analyze a whole string according to the task specification
-- | and return a list of vertices that are available from vertex 1
analyze          :: String -> [Int]

-- | Return the minimal vertex number in the input
minim            :: String -> Int

-- | Return the maximal vertex number in the input
maxim            :: String -> Int

-- | Iterate over neighbors of a vertex and add them one by one to a Graph.
processNeighbors :: Graph -> Int -> [Int] -> Graph

rInt = read

minim c = minimum (map rInt (words c))

maxim c = maximum (map rInt (words c))

processNeighbors g _ [] = g
processNeighbors g w1 (h:tl) = processNeighbors (addNeighbor g w1 h) w1 tl

processLine g c = processNeighbors g w1 w2
  where
    w2 = tail r
    w1 = head r
    r  = map rInt w
    w  = words c

analyze c = if lo <= 1 then getRoute ge lo hi 1 num else []
  where
    ge   = foldl processLine g (lines c)
    g    = createGraph (lo, hi) ini
    ini  = zip num (replicate maxBound [])
    num  = map rInt (words c)
    lo   = minim c
    hi   = maxim c

main = do
        args <- getArgs
        if null args
        then do
           contents <- getContents
           print (analyze contents)
        else do
           handle <- openFile (head args) ReadMode
           contents <- hGetContents handle
           if null (words contents)
           then putStrLn "Error: Empty file."
           else print (analyze contents)
           hClose handle
