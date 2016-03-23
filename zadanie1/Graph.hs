module Graph(Graph, createGraph, addNeighbor, getNeighbors, getRoute) where
import MyArray

type Graph   = Array Int [Int]
type Visited = Array Int Bool

-- |Create a graph with specified range of vertices.
createGraph  :: (Int, Int) -> Graph

-- |Add a specified neighbor to a specified vertex.
addNeighbor  :: Graph -> Int -> Int -> Graph

-- |Return neighbor list for a specfied vertex.
getNeighbors :: Graph -> Int -> [Int]

-- |Return the list of vertices available from a specified one.
getRoute     :: Graph -> Int -> Int -> Int -> [Int]

-- |Visit a specified vertex. [private]
--visit        :: Graph -> Visited -> Int -> Visited

-- |Visit each vertex from a list. [private]
--visitAll     :: Graph -> Visited -> [Int] -> Visited

-- The initial graph will contain all vertices between (lo, hi)
-- with empty neighbors list for each.
createGraph (lo, hi) = listArray (lo, hi) (replicate maxBound [])

addNeighbor g w1 w2 = update w1 l2 g
  where
    l2 = w2:(g ! w1)

getNeighbors g w = g ! w

bfs :: Graph -> Visited -> [Int] -> [Int] -> [Int]

getRoute g lo hi w = bfs g v1 [] [w]
  where
    v1 = update 1 True v
    v  = listArray (lo, hi) (replicate maxBound False)

bfs _ _ p []      = p
bfs g v p (h:tl)  = bfs g (mark v nbs) (h:p) (tl ++ addN v [] nbs)
    where
        nbs = getNeighbors g h

mark :: Visited -> [Int] -> Visited
mark v [] = v
mark v (h:nbs) = mark (update h True v) nbs

addN :: Visited -> [Int] -> [Int] -> [Int]
addN _ p []      = p
addN v p (h:tl) = if v ! h
                  then addN v p tl
                  else addN v (h:p) tl

{-
getRoute g lo hi w  = map snd (filter fst (zip l [lo..]))
    where
      l = elems (visit g v w)
      v = listArray (lo, hi) (replicate maxBound False)

visitAll _ v [] = v
visitAll g v (h:t) = visitAll g v' t where
  v' = visit g v h

visit g v w = if v ! w then v else visitAll g (update w True v) (g ! w)
-}
