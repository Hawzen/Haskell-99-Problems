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


prob19 :: [a] -> Int -> [a]
prob19 xs i =
        let len = length xs
            condition i n = mod (n+(-i)) len
            keys = map (condition i)  [0..len-1]
            dict = Map.fromList $ zip keys xs
        in [dict Map.! key | key <- [0..len-1]]


