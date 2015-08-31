module Exercises where

import Control.Monad
import Data.Array.IO
import System.Random
import Data.Functor

-- 이 파일은 모임 때 함께 풀어나갈 파일입니다.

-- Problem 1 ~ 10 : Lists --

-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast (x1:x2:[]) = x1
myButLast (x:xs) = myButLast xs


-- 3 
elementAt :: [a] -> Int -> a
elementAt (a:as) 0 = a
elementAt (a:as) n = elementAt as (n-1) 


-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (a:as) = 1 + myLength as


-- 5
myReverse :: [a] -> [a]
myReverse xs = myReverse' [] xs
               where myReverse' acc [] = acc
                     myReverse' acc (a:as) = myReverse' (a:acc) as

-- 6
isPalindrome:: Eq a => [a] -> Bool
isPalindrome xs = (myReverse xs) == xs

-- Problem 11 ~ 20 : Lists, continued --
-- Problem 21 ~ 28 : Lists again --


-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt a' as     1         = a':as
insertAt a' []     n         = if n == 1 then  [a']
                               else error "Invalid position"
insertAt a' (a:as) n         = a:insertAt a' as (n-1)

-- 22
range :: (Enum a) => a -> a -> [a]
range from to = enumFromTo from to

-- 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect as n = take n <$> shuffle as

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs' =  newListArray (1,n) xs'

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n

-- Problem 31 ~ 41 : Arithmetic --
-- Problem 46 ~ 50 : Logic and codes --
-- Problem 54A ~ 60 : Binary trees --
-- Problem 61 ~ 69 : Binary trees, continued --
-- Problem 70B ~ 73 : Multiway trees --
-- Problem 80 ~ 89 : Graphs --
-- Problem 90 ~ 94 : Miscellaneous problems --
-- Problem 95 ~ 99 : Miscellaneous problems, continued --
