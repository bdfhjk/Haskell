-- module MyArrayTests where
import MyArray
import Test.QuickCheck

prop_rangelo2 :: (Int,Int) -> Property
prop_rangelo2 b@(lo,hi) = (lo<=hi) ==> head(range b) == lo

prop_rangelo3 :: (Char,Char) -> Property
prop_rangelo3 b@(lo,hi) = (lo<=hi) ==> head(range b) == lo

-- Test.QuickCheck> quickCheck prop_rangelo2
-- +++ OK, passed 100 tests.


-- prop_acc :: Property
prop_acc = let m = 0::Int; n=10000 in 
  forAllBetween 0 n (\k -> property $
                   listArray (m,n) [m..n] ! k == k)
                                         
prop_elems :: [Char] -> Bool
prop_elems es = elems (listArray (1,length es) es) == es

prop_elems2 :: ((Char,Char),(Char,Char)) -> [Int] -> Property
prop_elems2 b es = size <= length es 
                   ==> elems (listArray b es) == take size es
                   where size = rangeSize b
                         
prop_array :: [Char] -> Bool
prop_array es = elems (array (1,length es) (zip [1..] es)) == es

{-
prop_update ::[Char] -> Char -> Property
prop_update cs c = forAllBetween 1 size (\k -> 
                   size > 0 ==> (update k c a ! k) == c) where
                     size = length cs
                     a = listArray (1,size) cs
-}

prop_update2 ::[Char] -> Char -> Property
prop_update2 cs c = forAllBetween 1 size (\k -> 
                    size > 0 ==> (a // [(k, c)] ! k) == c) where
                      size = length cs
                      a = listArray (1,size) cs

prop_update3 ::[Char] -> Bool
prop_update3 cs = elems (a // [])  == cs where 
  size = length cs
  a = listArray (1,size) cs
  
between y z x = (y<=x) && x<=z

forAllBetween :: Int -> Int -> (Int->Property) -> Property
-- forAllBetween m n p =  forAll (arbitrary `suchThat` (between m n)) p
forAllBetween m n p = forAll (choose (m,n)) p


qc :: Testable prop => prop -> IO ()
qc = quickCheck 
     -- quickCheckWith myArgs                                           

myArgs :: Args
myArgs = stdArgs 

writeln = putStrLn                                       
main = do                                         
  writeln "array"
  qc prop_array
  writeln "update"
--   qc prop_update
  qc prop_update2
  qc prop_update3
  writeln "rangelo2"
  qc prop_rangelo2
  writeln "rangelo3"
  qc prop_rangelo3
  writeln "acc"
  qc prop_acc
  writeln "elems"
  qc prop_elems
  qc prop_elems2
