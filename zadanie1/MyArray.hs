module MyArray ( Array, Ix, range, rangeSize, index, inRange,
                 listArray, (!), elems, array, update, (//)) where

class Ord a => Ix a where
    correct :: (a,a) -> Bool

    --Return the a-th element inside (b,c) range of indexes.
    fromPosition :: (a,a) -> Int -> a

    --Return the list of elements in range (a,b)
    range     :: (a,a) -> [a]

    --Get the numbers of elements in range (a, b)
    rangeSize :: (a,a) -> Int

    --Get the integer position of element a in range (a,b)
    index     :: (a,a) -> a -> Int

    --Check if the element a is in range (a, b)
    inRange   :: (a,a) -> a -> Bool


instance Ix Int where
    correct (lo, hi) = lo <= hi
    fromPosition (lo, _) no = lo + no
    range (lo, hi) = [lo .. hi]
    rangeSize (lo, hi) = max 0 (hi - lo + 1)
    index (lo, _) w = fromEnum w - fromEnum lo
    inRange (lo, hi) w
        | lo <= w && w <= hi = True
        | otherwise          = False

instance Ix Integer where
    correct (lo, hi) = lo <= hi
    fromPosition (lo, _) no = lo + toInteger no
    range (lo, hi) = [lo .. hi]
    rangeSize (lo, hi) = max 0 (fromIntegral(hi - lo + 1))
    index (lo, _) w = fromEnum w - fromEnum lo
    inRange (lo, hi) w
        | lo <= w && w <= hi = True
        | otherwise          = False

instance Ix Char where
    correct (lo, hi) = fromEnum lo <= fromEnum hi
    fromPosition (lo, _) no = toEnum (fromEnum lo + fromEnum no)
    range (lo, hi) = [lo .. hi]
    rangeSize (lo, hi) = max 0 (fromEnum hi - fromEnum lo + 1)
    index (lo, _) w = fromEnum w - fromEnum lo
    inRange (lo, hi) w
        | fromEnum lo <= fromEnum w && fromEnum w <= fromEnum hi  = True
        | otherwise                                               = False

instance (Ix a, Ix b) => Ix (a,b) where
    correct ((lo1, lo2), (hi1, hi2)) =
        correct (lo1, hi1) && correct (lo2, hi2)

    fromPosition ((lo1, lo2), (hi1, hi2)) no =
        (fromPosition (lo1, hi1) i1, fromPosition (lo2, hi2) i2)
        where
          r1 = rangeSize (lo2, hi2)
          i1 = quot no r1
          i2 = no - i1 * r1

    range ((lo1, lo2), (hi1, hi2))
        = [(a,b) | a <- range(lo1, hi1), b <- range(lo2, hi2)]

    rangeSize ((lo1, lo2),(hi1, hi2))
      = rangeSize (lo1, hi1) * rangeSize (lo2, hi2)

    index ((lo1, lo2), (hi1, hi2)) (mid1, mid2) = rs1 * i1 + i2
      where
        rs1 = rangeSize (lo2, hi2)
        i1 = index (lo1, hi1) mid1
        i2 = index (lo2, hi2) mid2

    inRange ((lo1, lo2), (hi1, hi2)) (mid1, mid2) =
      inRange (lo1, hi1) mid1 && inRange (lo2, hi2) mid2

data Array i e = Leaf i e | EmptyLeaf i
                | Node (Array i e) i (Array i e)
                | Root i i (Array i e)
                deriving Show

listArray   :: Ix i => (i, i) -> [e] -> Array i e
elems       :: Ix i => Array i e -> [e]
(!)         :: Ix i => Array i e -> i -> e
array       :: Ix i => (i, i) -> [(i, e)] -> Array i e
update      :: Ix i => i -> e -> Array i e -> Array i e
(//)        :: Ix i => Array i e -> [(i, e)] -> Array i e
listToPair  :: Ix i => (i, i) -> [e] -> [(i, e)]
makeArray   :: Ix i => (i, i) -> (i,i) -> Array i e

(!) (Root lo hi n) idx
    | inRange (lo, hi) idx  = (!) n idx
    | otherwise         = error "! - incorrect index"

(!) (Node n1 i n2) idx
    | idx <= i    = (!) n1 idx
    | otherwise   = (!) n2 idx

(!) (Leaf idx_old e) idx
    | idx_old == idx    = e
    | otherwise         = error "! - incorrect index"

(!) (EmptyLeaf _) _       = undefined

array (lo, hi) l
    |   correct (lo, hi)    =   (//) (Root lo hi (makeArray (lo, hi) (lo, hi))) l
    |   otherwise        =   Root lo hi (EmptyLeaf lo)

listArray  (lo, hi) l
    |   correct (lo, hi)
        =  (//) (Root lo hi (makeArray (lo, hi) (lo, hi))) (listToPair (lo, hi) l)
    |   otherwise   =   Root lo hi (EmptyLeaf hi)

update idx e (Root lo hi n)
    | inRange (lo, hi) idx       = Root lo hi (update idx e n)
    | otherwise               = error "update - incorrect index"

update idx e (Node n1 sr n2)
    | idx <= sr   = Node (update idx e n1) sr n2
    | otherwise   = Node n1 sr (update idx e n2)

update idx e (Leaf idx_old _) = Leaf idx e
--    | idx == idx_old
--    | otherwise        = error "update - incorrect index"

update idx e (EmptyLeaf idx_old) = Leaf idx e

(//) (Root lo hi n) [] = Root lo hi n
(//) (Root lo hi n) ((i, e):tl)
    | inRange (lo, hi) i     = (//) (Root lo hi (update i e n)) tl
    | otherwise              = Root lo hi n
(//) _ _ = error "// - used on non-root node"


listToPairR (_, _) [] idx = []

listToPairR (lo, hi) (h:tl) idx
    | idx == rangeSize (lo, hi)    = []
    | otherwise   =  (i, h) : listToPairR (lo, hi) tl (idx+1)
    where
        i = fromPosition (lo, hi) idx

listToPair (lo, hi) l = listToPairR (lo, hi) l 0

makeArray (loi, hii) (lo, hi)
    | lo == hi           = EmptyLeaf lo
    | correct (lo, hi)
        = Node (makeArray (loi, hii) (lo, sr))
                sr
               (makeArray (loi, hii) (srn, hi))
    | otherwise          = EmptyLeaf hi
    where
      lo_idx = index (loi, hii) lo
      hi_idx = index (loi, hii) hi
      sr  = fromPosition (loi, hii) ((quot (hi_idx + lo_idx) 2))
      srn = fromPosition (loi, hii) ((quot (hi_idx + lo_idx) 2)+1)

elems (Root _ _ n)        = elems n
elems (Leaf _ e)          = [e]
elems (EmptyLeaf _)       = []
elems (Node a1 _ a2)      = elems a1 ++ elems a2
