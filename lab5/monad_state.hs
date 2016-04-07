import Control.Monad.State

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
type S a = Int -> a

renumberTree :: Tree a -> S Int -> Tree Int
main :: IO()


renumberTree Empty = return Empty
renumberTree (Node _ tl tr) = do
  d <- get
  tl' <- put (d+1) (renumberTree tl)
  tr' <- put (d+1) (renumberTree tr)
  return (Node d tl' tr')

main = print $ (renumberTree Empty 0 )
