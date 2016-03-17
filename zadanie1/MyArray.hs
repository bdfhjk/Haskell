class Ord a => Ix a where
    range     :: (a,a) -> [a]
    index     :: (a,a) -> a -> Int
    inRange   :: (a,a) -> a -> Bool
    rangeSize :: (a,a) -> Int

instance Ix Int where
    rangeSize (lo,hi) = hi - lo + 1
    range (lo,hi) = [lo .. hi]
    inRange (lo, hi) w
        | lo <= w && w <= hi = True
        | otherwise          = False
    index (lo, hi) w = fromEnum(w) - fromEnum(lo)

instance Ix Integer where
    rangeSize (lo,hi) = fromIntegral(hi - lo + 1)
    range (lo,hi) = [lo .. hi]
    inRange (lo, hi) w
        | lo <= w && w <= hi = True
        | otherwise          = False
    index (lo, hi) w = fromEnum(w) - fromEnum(lo)

instance Ix Char where
    rangeSize (lo,hi) = fromEnum(hi) - fromEnum(lo) + 1
    range (lo,hi) = [lo .. hi]
    inRange (lo, hi) w
        | fromEnum(lo) <= fromEnum(w) && fromEnum(w) <= fromEnum(hi)    = True
        | otherwise                                                     = False
    index (lo, hi) w = fromEnum(w) - fromEnum(lo)

instance (Ix a, Ix b) => Ix (a,b) where
    --rangeSize ((lo1, hi1), (lo2, hi2)) = rangeSize (lo1, hi1)
    --rangeSize ((lo1, hi1),(lo2, hi2)) = rangeSize (lo1, hi1) -- * rangeSize (lo2, hi2) 




 
