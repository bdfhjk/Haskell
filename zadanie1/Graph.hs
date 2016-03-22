module Graph(createGraph, addNeighbor, getNeighbors, getRoute) where
import MyArray

type Graph   = Array Int [Int]
type Visited = Array Int Bool

-- Create a graph with specified range of vertices
createGraph  :: (Int, Int) -> Graph

-- Add a specified neighbor to a specified vertex
addNeighbor  :: Graph -> Int -> Int -> Graph

-- Return neighbor list for a specfied vertex
getNeighbors :: Graph -> Int -> [Int]

-- Return the list of vertices available from a specified one
getRoute     :: Graph -> Int -> Int -> Int -> [Int]

-- Visit a specified vertex
visit        :: Graph -> Visited -> Int -> Visited

-- Visit each vertex from a list
visitAll     :: Graph -> Visited -> [Int] -> Visited

createGraph (lo, hi) = listArray (lo, hi) (replicate maxBound [])

addNeighbor g w1 w2 = update w1 l2 g
  where
    l2 = w2:(g ! w1)

getNeighbors g w = g ! w

getRoute g lo hi w  = map snd (filter fst (zip l [1..]))
    where
      l = elems (visit g v w)
      v = listArray (lo, hi) (replicate maxBound False)

visitAll _ v [] = v
visitAll g v (h:t) = visitAll g v' t where
  v' = visit g v h

visit g v w = if v ! w then v else visitAll g (update w True v) (g ! w)
