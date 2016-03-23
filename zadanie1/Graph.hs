module Graph(Graph, createGraph, addNeighbor, getNeighbors, getRoute) where
import MyArray
import Data.List
import Data.Set

type Graph   = Array Int [Int]

-- |Create a graph with specified range of vertices.
createGraph  :: (Int, Int) -> [(Int, [Int])]-> Graph

-- |Add a specified neighbor to a specified vertex.
addNeighbor  :: Graph -> Int -> Int -> Graph

-- |Return neighbor list for a specfied vertex.
getNeighbors :: Graph -> Int -> [Int]

-- |Return the list of vertices available from a specified one.
getRoute     :: Graph -> Int -> Int -> Int -> [Int] -> [Int]

-- |Perform BFS algorithm over the graph and return visited nodes
-- |First list is a list of visited previously nodes
-- |Second list is a list of nodes in queue to be processed
bfs :: Graph -> Set Int -> [Int] -> [Int] -> [Int]

-- |Mark list of vertices as visited in an array
mark :: Set Int -> [Int] -> Set Int

-- |Process list and return only non-visited vertices
addN :: Set Int -> [Int] -> [Int] -> [Int]


createGraph (lo, hi) = array (lo, hi)

addNeighbor g w1 w2 = update w1 l2 g
  where
    l2 = w2:(g ! w1)

getNeighbors g w = g ! w

getRoute g _ _ w _ = sort (bfs g v [] [w])
  where
    v  = fromList [1]

bfs _ _ p []      = p
bfs g v p (h:tl)  = bfs g (mark v nbs) (h:p) (tl ++ addN v [] nbs)
    where
        nbs = getNeighbors g h

mark v []      = v
mark v (h:nbs) = mark (Data.Set.insert h v) nbs

addN _ p []      = p
addN v p (h:tl)  = if member h v
                  then addN v p tl
                  else addN v (h:p) tl
