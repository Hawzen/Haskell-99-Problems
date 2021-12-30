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


prob21 :: a -> [a] -> Int -> [a]
prob21 new xs m
        | length xs < m = error "Ya broke it! list too small"
        | m <= 0 = error "Ya broke it! bad index"
        | m == length xs = xs ++ [new]
        | otherwise =
            let list = (zip [1..] xs) 
                removeM = (\(i, x) acc -> if i==m then new:x:acc else x:acc)
            in (foldr removeM [] list)


