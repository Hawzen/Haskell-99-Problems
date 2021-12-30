gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = let (q, r) = a `divMod` b
           in gcd' b r

coprime :: Integer -> Integer -> Bool
coprime a b = gcd' a b == 1