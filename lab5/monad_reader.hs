import Control.Monad.Reader

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

type R a = Int -> a
renumber :: Tree a -> R(Tree Int)

renumber Empty = return Empty
renumber (Node _ tl tr) = do
  d <- ask
  tl' <- local (+1) (renumber tl)
  tr' <- local (+1) (renumber tr)
  return (Node d tl' tr')
