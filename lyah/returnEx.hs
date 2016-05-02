main = do
  return ()
  return "HAHAHA"
  line <- getLine
  return "BBB"
  return 4
  putStrLn line
  a <- return "test"
  b <- return "test2"
  putStrLn $ a ++ " " ++ b
