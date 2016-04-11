import Control.Monad.State

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
type S a = State Int a

renumberTree :: Tree a -> S (Tree Int)
main :: IO()

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Empty = Node a Empty Empty
insert a (Node b l r) = Node b l2 r2 where
    l2 = if a < b then insert a l else l
    r2 = if a > b then insert a r else r

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Empty

toList :: Tree a -> [a]
toList Empty = []
toList (Node a l r) = toList l ++ [a] ++ toList r

renumberTree Empty = return Empty
renumberTree (Node _ tl tr) = do
  tl' <- renumberTree tl
  d <- get
  put (d+1)
  tr' <- renumberTree tr
  return (Node d tl' tr')

main = print $ toList $ evalState (renumberTree $ fromList "Learn Haskell") 0
