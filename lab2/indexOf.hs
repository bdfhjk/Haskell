indexOf :: Char -> String -> Maybe Int
indexOf2 :: Char -> String -> Int -> Maybe Int
positions :: Char -> String -> [Int]

indexOf2 a [] b = Nothing
indexOf2 a (x:xs) b 
    | a == x        = Just b
    | otherwise     = indexOf2 a xs (b+1)

indexOf a l = indexOf2 a l 0

positions2 a [] b n = b
positions2 a (x:xs) b n
    | a == x        = positions2 a xs (n:b) (n+1)
    | otherwise     = positions2 a xs b (n+1)
positions a l = reverse(positions2 a l [] 0)

