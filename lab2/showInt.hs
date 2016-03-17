import Data.Char

showInt :: Int -> String
intToChar :: Int -> Char

intToChar a 
    | a == 1  = '1'
    | a == 2  = '2'
    | a == 3  = '3'
    | a == 4  = '4'
    | a == 5  = '5'
    | a == 6  = '6'
    | a == 7  = '7'
    | a == 8  = '8'
    | a == 9  = '9'
    | a == 0  = '0'

showInt a  
    | a < 0         = "-" ++ showInt (-a)
    | a <= 9        = [intToChar a]
    | otherwise     = showInt (div a 10) ++ showInt (mod a 10)
