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


prob26 :: Int -> [a] -> [[a]]
prob26 0 _ = [[]]
prob26 1 xs = [[x] | x <- xs]
prob26 level xs = let subComb i = prob26 (level-1) $ snd $ splitAt i xs
                  in [x:list | (i, x) <- zip [1..] xs, list <- subComb i]


