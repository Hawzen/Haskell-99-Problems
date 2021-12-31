
(<=>) :: Bool -> Bool -> Bool
a <=> b = a && b

(>=<) :: Bool -> Bool -> Bool
a >=< b = a || b

infixl 5 >=<
infixl 6 <=>

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