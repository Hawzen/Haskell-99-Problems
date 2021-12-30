
and' :: Bool -> Bool -> Bool
and' a b = a && b

or' :: Bool -> Bool -> Bool
or' a b = a || b

nand' :: Bool -> Bool -> Bool
nand' a b = not $ a && b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ a || b

xor' :: Bool -> Bool -> Bool
xor' a b = not (a && b || not a && not b)

impl' :: Bool -> Bool -> Bool
impl' a b = not $ a && not b

equ' :: Bool -> Bool -> Bool
equ' a b = a == b

-- I have no idea what the description for table meant, so I copied their implementation
table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
                                | a <- [True, False], b <- [True, False]]
