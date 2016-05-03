import System.Random
import Control.Monad.State

main :: IO()


threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- random get
  b <- random get
  c <- random get
  return (a,b,c)

main = print $ runState threeCoins (mkStdGen 33)
