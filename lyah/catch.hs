import System.Environment
import System.IO.Error
import Control.Exception

main :: IO()

main = catch toTry handler

toTry :: IO()
toTry = do
  (fileName:_) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length ( lines contents)) ++ " lines"

handler :: IOError -> IO ()

handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
