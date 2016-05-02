import Control.Monad
import Data.Char

main = do
  sequence $ map print [1,2,3,4,5]
  mapM print [1,2,3]
  mapM_ print [1,2,3]
  forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
