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


prob17 :: Int -> [a] -> ([a], [a])
prob17 m = 
        let sndAppend x acc = (fst acc, x:(snd acc))
            fstAppend x acc= (x:(fst acc), snd acc)
            checkAndAppend = (\acc (i, x) -> if m<i then sndAppend x acc else fstAppend x acc)
            rev acc = (reverse (fst acc), reverse (snd acc))
        in rev . foldl checkAndAppend ([], []) . zip [1,2..]


