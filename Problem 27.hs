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


