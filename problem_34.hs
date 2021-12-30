import Data.ByteString (foldl')
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = let (q, r) = a `divMod` b
           in gcd' b r

coprime :: Integer -> Integer -> Bool
coprime a b = gcd' a b == 1

phi :: Integer -> Int 
phi i 
    | i >= 1 = length $ filter (coprime i) [1..i]
    | otherwise = error "Phi is defined on numbers greater than or equal 1"