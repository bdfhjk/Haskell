import Control.Monad

inits :: [a] -> [[a]]
inits [] = return []
inits (x:xs) = return [] `mplus` do
  ys <- inits xs
  (return (x:ys))

allPairs :: [a] -> [a] -> [[a]]
allPairs xs ys = [[x,y] | x <- xs, y <- ys]

allPairs2 :: [Int] -> [Int] -> [[Int]]
allPairs2 xs ys = do
  x <- xs
  y <- ys
  return [x,y]

allCombinations :: [[a]] -> [[a]]
allCombinations [] = [[]]
allCombinations (xs:xss) = do
  x <- xs
  l <- allCombinations xss
  return (x:l)
