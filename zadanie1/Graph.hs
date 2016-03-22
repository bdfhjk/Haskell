module Graph where
import MyArray

type Graph = Array Int [Int]

createGraph  :: (Int, Int) -> Graph
addNeighbor  :: Graph -> Int -> Int -> Graph
getNeighbors :: Graph -> Int -> [Int]

createGraph (lo, hi) = listArray (lo, hi) (replicate maxBound [])

addNeighbor g w1 w2 = update w1 l2 g
  where
    l2 = w2:(g ! w1)

getNeighbors g w = g ! w
