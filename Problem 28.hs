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


