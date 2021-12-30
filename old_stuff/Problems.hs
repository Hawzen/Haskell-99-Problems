{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- List of problems: https://wiki.haskell.org/99_questions/1_to_10
module Problems where

import Data.List
import Data.Char
import Debug.Trace
import System.Random
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Map (insert)
import qualified Data.Set as Set


data Element a = Single a | Multiple Int a  deriving Show
instance (Eq m) => Eq (Element m) where
    (Multiple i x) == (Multiple j y) = i == j && x == y
    Single x == (Multiple 1 y) = x == y
    Single x == Single y = x == y 
    _ == _ = False


-- Problem 1
prob1 :: [a] -> a
prob1 [] = error "Empty list"
prob1 xs = last xs


-- Problem 2
prob2 :: [a] -> a
prob2 [] = error "Empty list"
prob2 xs
    | length xs == 1 = head xs
    | otherwise = let x = init xs in last x


-- Problem 3
prob3 :: [a] -> Int -> a
prob3 [] i = error "Empty list"
prob3 xs i = case len of True -> xs !! (i - 1)
                         False -> error "Index out of range"
    where len = length xs >= i


-- Problem 5
prob5 :: [a] -> [a]
prob5 [] = []
prob5 xs = [last xs] ++ prob5 (init xs)


prob4 :: [a] -> Int
prob4 = length


-- Problem 6
prob6 :: (Eq a) => [a] -> Bool
prob6 xs = xs == prob5 xs


-- Problem 7 
data NestedList a = Elem a | List [NestedList a]
prob7 :: NestedList a -> [a]
prob7 (Elem a) = [a]
prob7 (List (x:xs)) = prob7 x ++ prob7 (List xs)
prob7 (List []) = []


-- Problem 8
prob8 :: (Eq a) => [a] -> [a]
prob8 xs = foldr (\x acc -> if (head acc) == x then acc else x:acc) [last xs] (init xs)


-- Problem 9
prob9 :: (Eq a) => [a] -> [[a]]
prob9 [] = [[]]
prob9 (y:ys) = reverse $ foldl (\acc@(hd:tl) x -> if head hd == x then (x:hd):tl else [x]:acc) [[y]] ys


-- Problem 10
prob10 :: (Eq a) => [a] -> [(Int, a)]
prob10 xss = map (\xs -> (length xs, head xs)) $ prob9 xss


-- Problem 11
prob11 :: Eq a => [a] -> [Element a]
prob11 xs = map formatter $ prob10 xs
        where 
          formatter (1, x) = Single x
          formatter (i, x) = Multiple i x


-- Problem 12
prob12 :: [Element a] -> [a]
prob12 = foldr deformatter []
        where
          deformatter (Single a) acc     = a:acc
          deformatter (Multiple i a) acc = replicate i a ++ acc


-- Problem 13
prob13 :: Eq a => [a] -> [Element a]
prob13 = foldr appender []
        where
            appender x [] =  [Single x] 
            appender x (Single h:acc) = 
                                if h == x
                                    then Multiple 2 x:acc
                                    else Single x:Single h:acc
            appender x (h@(Multiple i _):acc) =
                                if Single x == h 
                                    then Multiple (i+1) x:acc
                                    else Single x:(h:acc)


-- Problem 14
prob14 :: [a] -> [a]
prob14 = foldl1 (++) . map (\x -> [x,x])


-- Problem 15
prob15 :: (Enum a) => Int -> [a] -> [a]
prob15 i =  let replicate x = take i [x,x..] 
            in foldl (\acc x -> acc ++ replicate x) [] 


-- Problem 16
prob16 :: Int -> [a] -> [a]
prob16 m = 
            let condition i = (i `mod` m) == 0
                checkAndExecute = (\acc (i, x) -> if condition i then acc else x:acc)
            in reverse . foldl checkAndExecute [] . zip [1,2..]


-- Problem 17
prob17 :: Int -> [a] -> ([a], [a])
prob17 m = 
        let sndAppend x acc = (fst acc, x:(snd acc))
            fstAppend x acc= (x:(fst acc), snd acc)
            checkAndAppend = (\acc (i, x) -> if m<i then sndAppend x acc else fstAppend x acc)
            rev acc = (reverse (fst acc), reverse (snd acc))
        in rev . foldl checkAndAppend ([], []) . zip [1,2..]


-- Problem 18
prob18 :: [a] -> Int -> Int -> [a]
prob18 xs i j = 
        let condition (_, m) = i <= m && m <= j
        in map fst $  filter condition (zip xs [1..])


-- Problem 19
prob19 :: [a] -> Int -> [a]
prob19 xs i =
        let len = length xs
            condition i n = mod (n+(-i)) len
            keys = map (condition i)  [0..len-1]
            dict = Map.fromList $ zip keys xs
        in [dict Map.! key | key <- [0..len-1]]


-- Problem 20
prob20 :: Int -> [a] -> (a, [a])
prob20 m xs
        | length xs <= m = error "Ya broke it! list too small"
        | m <= 0 = error "Ya broke it! bad index"
        | otherwise =
            let list = zip [1..] xs
                acc = (xs !! (m-1), [])
                removeM = (\(i, x) acc -> if i==m-1 then acc else (fst acc, x:snd acc))
            in (foldr removeM acc list)


-- Problem 21
prob21 :: a -> [a] -> Int -> [a]
prob21 new xs m
        | length xs < m = error "Ya broke it! list too small"
        | m <= 0 = error "Ya broke it! bad index"
        | m == length xs = xs ++ [new]
        | otherwise =
            let list = (zip [1..] xs) 
                removeM = (\(i, x) acc -> if i==m then new:x:acc else x:acc)
            in (foldr removeM [] list)


-- Problem 22
prob22 :: Int -> Int -> [Int]
prob22 i j = [i..j]


-- Problem 23
prob23 :: (Integral i) => [a] -> i -> IO [a]
prob23 xs i = do
                gen <- getStdGen
                let indices = genericTake i $ randomRs (0, length xs - 1) gen
                return $ map (\ind -> xs !! ind) indices


-- Problem 24
prob24 :: (Num a, Enum a, Integral i) => i -> a -> IO [a]
prob24 i n = prob23 [1..n] i


-- Problem 25
prob25 :: [a] -> IO [a]
prob25 xs = do
                gen <- getStdGen
                let bools = take (length xs) $ randoms gen :: [Bool]
                    appender  (x, b) (fals, tru) = 
                                                    if b
                                                    then (fals, x:tru) 
                                                    else (x:fals, tru)
                    cat (fals, tru) = fals ++ tru
                return $ cat $ foldr appender ([], []) $ zip xs bools


-- Problem 26
prob26 :: Int -> [a] -> [[a]]
prob26 0 _ = [[]]
prob26 1 xs = [[x] | x <- xs]
prob26 level xs = let subComb i = prob26 (level-1) $ snd $ splitAt i xs
                  in [x:list | (i, x) <- zip [1..] xs, list <- subComb i]


-- Problem 27
prob27 :: (Eq a) => [Int] -> [a] -> [[[a]]]
prob27 [] _ = [[[]]]
prob27 (i:is) xs = let combinations = prob26 i xs
                       appender comb list = if null $ head list
                                              then [comb]
                                              else [comb] ++ list
                   in if length is + 1 /= length xs
                        then error "Mismatch Length"
                        else [appender comb list| comb <- combinations,
                                                  list <- prob27 is $ (xs \\ comb)]


-- Problem 28 multiple
prob28 :: Ord a => [[a]] -> [[a]]
prob28 xss = sortBy compare xss


prob28' :: Ord a => [[a]] -> [[a]]
prob28' xss = let lookIncrement x acc = 
                      let l = length x
                          b = maybe 0 id $ Map.lookup l acc
                      in Map.insert l (b+1) acc
                  hashmap = foldr lookIncrement (Map.fromList [(0, 0)]) xss
                  hashCompare a b = compare (Map.lookup (length a) hashmap) $ (Map.lookup (length b) hashmap)
              in sortBy hashCompare xss


-- Problem 50
data Tree a = Leaf a | Node a (Tree a) (Tree a)
              deriving (Show, Read, Eq)

-- instance Ord (Tree(Int, String)) where
--     compare (Leaf (i, _))       (Leaf (j, _))       = compare i j
--     compare (Leaf (i, _))     n@(Node (j, _) _ _)   = compare i (freq n)
--     compare n@(Node (i, _) _ _)   (Leaf (j, _))       = compare (freq n) j
--     compare n1 n2 = compare (freq n1) (freq n2)


instance Semigroup (Tree (Int, String)) where
    (<>) (Leaf (i, x))     (Leaf (j, y))     = Leaf (i+j, x ++ y)
    (<>) (Leaf (i, x))     (Node (j, y) _ _) = Leaf (i+j, x ++ y)
    (<>) (Node (i, x) _ _) (Leaf (j, y))     = Leaf (i+j, x ++ y)
    (<>) (Node (i, x) _ _) (Node (j, y) _ _) = Leaf (i+j, x ++ y)


instance Monoid (Tree (Int, String)) where  
    mempty = Leaf (0, "")

    orig@(Node (i, x) _ _) `mappend` (Leaf (0, ""))         = orig
    (Leaf (0, ""))         `mappend` orig@(Node (i, x) _ _) = orig

    (Leaf (i, x))     `mappend` (Leaf (j, y))        =
                             Leaf (i+j, x ++ y) 

    (Leaf (i, x))     `mappend` (Node (j, y) l r)    = 
                             Leaf (i+j, x ++ y) `mappend` l `mappend` r

    (Node (i, x) l r) `mappend` (Leaf (j, y))        =
                             Leaf (i+j, x ++ y)  `mappend` l `mappend` r

    (Node (i, x) l r) `mappend` (Node (j, y) l2 r2)  = 
                             Leaf (i+j, x ++ y) `mappend` l `mappend` r
                                                `mappend` l2 `mappend` r2


collectSeperate :: Tree (Int, String) -> String -> [((Int, String), String)]
collectSeperate (Leaf (i, s)) codes = [((i, s), codes)]
collectSeperate node@(Node (i, s) l r) codes = 
                            (collectSeperate l ('L':codes)) ++
                            (collectSeperate r ('R':codes))


collectOne :: Tree (Int, String) -> (Int, String)
collectOne (Leaf (i, s)) = (i, s)
collectOne node@(Node (i, s) l r) = collectOne $ l `mappend` r

freq :: Tree (Int, String) -> Int
freq (Leaf (i, _)) = i
freq node = fst $ collectOne node


stackSort :: [Tree (Int, String)] -> [Tree (Int, String)]
stackSort xs = sortBy (\a b-> compare (freq a) (freq b)) xs


pluck :: [Tree (Int, String)] -> [Tree (Int, String)]
pluck (first:second:xs) = (Node (0, "") first second):xs


pluckAll :: [Tree (Int, String)] -> Tree (Int, String)
pluckAll (fulltree:[]) = fulltree
pluckAll xs = pluckAll $ pluck $ stackSort xs


prob50 :: [(Char, Int)] -> [(Char, String)]
prob50 xs = let stack = map (\(ch,freq) -> Leaf (freq,[ch])) xs
                fulltree = pluckAll stack
             in map (\((i, s:_), code) -> (s, reverse code)) $ 
                                    collectSeperate fulltree ""
