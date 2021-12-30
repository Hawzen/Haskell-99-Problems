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


prob20 :: Int -> [a] -> (a, [a])
prob20 m xs
        | length xs <= m = error "Ya broke it! list too small"
        | m <= 0 = error "Ya broke it! bad index"
        | otherwise =
            let list = zip [1..] xs
                acc = (xs !! (m-1), [])
                removeM = (\(i, x) acc -> if i==m-1 then acc else (fst acc, x:snd acc))
            in (foldr removeM acc list)


