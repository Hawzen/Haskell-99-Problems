gray :: Num a => Int -> [[a]]
gray n = sequence $ replicate n [0, 1]