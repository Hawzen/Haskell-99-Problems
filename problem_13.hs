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


