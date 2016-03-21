module MyArray ( Array, Ix, range, rangeSize, index, inRange) where

class Ord a => Ix a where

    --Return the a-th element inside (b,c) range of indexes.
    fromPosition :: (a,a) -> Int -> a

    --Return the list of elements in range (a,b) from c inclusive
    rangeFrom :: (a,a) -> a -> [a]

    --Return the list of elements in range (a,b)
    range     :: (a,a) -> [a]

    --Get the numbers of elements in range (a, b)
    rangeSize :: (a,a) -> Int

    --Get the integer position of element a in range (a,b)
    index     :: (a,a) -> a -> Int

    --Check if the element a is in range (a, b)
    inRange   :: (a,a) -> a -> Bool


instance Ix Int where
    fromPosition (lo, _) no = lo + no
    rangeFrom (_, hi) mid = [mid .. hi]
    range (lo, hi) = [lo .. hi]
    rangeSize (lo, hi) = hi - lo + 1
    index (lo, _) w = fromEnum w - fromEnum lo
    inRange (lo, hi) w
        | lo <= w && w <= hi = True
        | otherwise          = False

instance Ix Integer where
    fromPosition (lo, _) no = lo + toInteger no
    rangeFrom (_, hi) mid = [mid .. hi]
    range (lo, hi) = [lo .. hi]
    rangeSize (lo, hi) = fromIntegral(hi - lo + 1)
    index (lo, _) w = fromEnum w - fromEnum lo
    inRange (lo, hi) w
        | lo <= w && w <= hi = True
        | otherwise          = False

instance Ix Char where
    fromPosition (lo, _) no = toEnum (fromEnum lo + fromEnum no)
    rangeFrom (_, hi) mid = [mid .. hi]
    range (lo, hi) = [lo .. hi]
    rangeSize (lo, hi) = fromEnum hi - fromEnum lo + 1
    index (lo, _) w = fromEnum w - fromEnum lo
    inRange (lo, hi) w
        | fromEnum lo <= fromEnum w && fromEnum w <= fromEnum hi  = True
        | otherwise                                               = False

instance (Ix a, Ix b) => Ix (a,b) where
    fromPosition ((lo1, lo2), (hi1, hi2)) no =
        (fromPosition (lo1, hi1) i1, fromPosition (lo2, hi2) i2)
        where
          r1 = rangeSize (lo1, hi1)
          i1 = quot no r1
          i2 = no - i1 * r1

    rangeFrom ((lo1, lo2), (hi1, hi2)) (mid1, mid2)
        | imid1 == ihi1 && imid2 == ihi2 = [(mid1, mid2)]
        | imid1 < ihi1  && imid2 == ihi2
          = (mid1, mid2):rangeFrom ((lo1, lo2), (hi1, hi2)) (mid1n, lo2)
        | otherwise
          = (mid1, mid2):rangeFrom ((lo1, lo2), (hi1, hi2)) (mid1, mid2n)
        where
          ihi1   = rangeSize (lo1, hi1) - 1
          ihi2   = rangeSize (lo2, hi2) - 1
          imid1  = index (lo1, hi1) mid1
          imid2  = index (lo2, hi2) mid2
          mid2n  = fromPosition (lo2, hi2) (imid2 + 1)
          mid1n  = fromPosition (lo1, hi1) (imid1 + 1)

    range ((lo1, lo2), (hi1, hi2))
      = rangeFrom ((lo1, lo2), (hi1, hi2)) (lo1, lo2)

    rangeSize ((lo1, lo2),(hi1, hi2))
      = rangeSize (lo1, hi1) * rangeSize (lo2, hi2)

    index ((lo1, lo2), (hi1, hi2)) (mid1, mid2) = rs1 * i1 + i2
      where
        rs1 = rangeSize (lo1, hi1)
        i1 = index (lo1, hi1) mid1
        i2 = index (lo2, hi2) mid2

    inRange ((lo1, lo2), (hi1, hi2)) (mid1, mid2) =
      inRange (lo1, hi1) mid1 && inRange (lo2, hi2) mid2

data Array i e = Leaf i e | EmptyLeaf i
                | Node (Array i e) i (Array i e)
                | Root i i (Array i e)
                deriving Show
listArray   :: (Ix i, Show i, Show e) => (i, i) -> [e] -> Array i e
elems       :: (Ix i, Show i, Show e) => Array i e -> [e]
(!)         :: (Ix i, Show i, Show e) => Array i e -> i -> e
array       :: (Ix i, Show i, Show e) => (i, i) -> [(i, e)] -> Array i e
update      :: (Ix i, Show i, Show e) => i -> e -> Array i e -> Array i e
(//)        :: (Ix i, Show i, Show e) => Array i e -> [(i, e)] -> Array i e
listToPair  :: (Ix i, Show i, Show e) => (i, i) -> [e] -> [(i, e)]
makeArray   :: (Ix i, Show i, Show e) => (i, i) -> Array i e

{-
instance (Show i, Show e) => Show (Array i e) where
    show (EmptyLeaf idx) = "EmptyLeaf " ++ show idx
    show (Leaf idx a) = "Leaf "
    show (Root lo hi n) = "Root "
    show (Node n1 e n2) = "Node " -}

(!) (Root lo hi n) idx
    | idx >= lo && idx <= hi  = (!) n idx
    | otherwise         = error "! - incorrect index"

(!) (Node n1 i n2) idx
    | idx <= i    = (!) n1 idx
    | otherwise   = (!) n2 idx

(!) (Leaf idx_old e) idx
    | idx_old == idx    = e
    | otherwise         = error "! - incorrect index"

(!) (EmptyLeaf _) _       = undefined

array (lo, hi) =
    (//) (makeArray (lo, hi))

listArray  (lo, hi) l =
    Root lo hi ((//) (makeArray (lo, hi)) (listToPair (lo, hi) l))

update idx e (Root lo hi n)
    | idx >= lo && idx <= hi  = Root lo hi (update idx e n)
    | otherwise               = error "update - incorrect index"

update idx e (Node n1 sr n2)
    | idx <= sr   = Node (update idx e n1) sr n2
    | otherwise   = Node n1 sr (update idx e n2)

update idx e (Root lo hi n)
    | idx >= lo && idx <= hi  = Root lo hi (update idx e n)

update idx e (Leaf idx_old _)
    | idx == idx_old   = Leaf idx e
    | otherwise        = error "update - incorrect index"

update idx e (EmptyLeaf idx_old)
    | idx == idx_old    = Leaf idx e
    | otherwise         = error "update - incorrect index"

(//) (Root lo hi n) [] = Root lo hi n
(//) (Root lo hi n) ((i, e):tl)
    | i >= lo && i <= hi  = (//) (Root lo hi (update i e n)) tl
    | otherwise           = error "// - incorrect index"

(//) x y = error (show x)
--(//) (n1) _ = n1

listToPair (_, _) [] = []
listToPair (lo, hi) (h:tl)
    | lo == hi    = [(lo, h)]
    | otherwise   = (lo ,h) : listToPair (lon, hi) tl
    where
      lon = fromPosition (lo, hi) 1

makeArray (lo, hi)
    | lo == hi    = EmptyLeaf lo
    | otherwise   = Node (makeArray (lo, sr)) sr (makeArray (srn, hi))
    where
      sr  = fromPosition (lo, hi) (quot (rangeSize (lo, hi)) 2 - 1)
      srn = fromPosition (lo, hi) (quot (rangeSize (lo, hi)) 2)

elems (Root _ _ n)        = elems n
elems (Leaf _ e)          = [e]
elems (EmptyLeaf _)       = []
elems (Node a1 _ a2)      = elems a1 ++ elems a2

main :: IO()
main = do
       print (rangeSize(('a', ('a', 5::Int)),('z',('z',10::Int))))
       print (range(('a', ('a', 1::Int)),('c',('b', 2::Int))))
