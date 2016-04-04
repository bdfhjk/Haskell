import Prelude hiding(Functor(..))

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Ord)
singleton :: a -> Tree a
singleton x = Node x Empty Empty

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Node a l r) = (show l) ++ " " ++ (show a) ++ " " ++ (show r)

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    (Node a l r) == (Node b l2 r2) = (a == b) && (l == l2) && (r == r2)

head (Node a l r) = a
lhead (Node a Empty r) = a
lhead (Node a l r) = lhead l

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

toList :: Tree a -> [a]

toList Empty = []
toList (Node a l r) = toList l ++ [a] ++ toList r

insert :: (Ord a) => a -> Tree a -> Tree a
insert a Empty = Node a Empty Empty
insert a (Node b l r) = Node b l2 r2 where
    l2 = if (a < b) then insert a l else l
    r2 = if (a > b) then insert a r else r

sized :: Tree a -> Int
sized Empty = 0
sized (Node a l r) = (max (sized l) (sized r)) + 1

{-
rotate :: Tree a -> Tree a
rotate Empty = Empty
rotate (Node a Empty Node(c lx rx)) = Node b l2 r2 where
    b = if (sized r > 2) then c else a
    l2 = if (sized r > 2) then
-}

contains :: (Ord a) => a -> Tree a -> Bool
contains a Empty = False
contains a (Node b l r)
    | a == b    = True
    | a < b     = contains a l
    | a > b     = contains a r

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList (h:t) = insert h (fromList t)
