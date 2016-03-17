triads l = [(x,y,z) | x <- [1..l], y <- [x..l], z <- [y..l], 
    x^2 + y^2 == z^2,
    gcd x y == 1]
