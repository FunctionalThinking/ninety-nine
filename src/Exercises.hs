module Exercises where

import Control.Monad
import Data.Array.IO
import Data.Functor
import System.Random (randomRIO)
import Data.List (group, subsequences)


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


-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List as) = concatMap flatten as

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (a:as) = a : compress (dropWhile (==a) as)


-- 9
pack :: (Eq a) => [a] -> [[a]]
pack = group -- span, takeWhile/dropWhile

-- 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = let packed = pack xs
            in map (\xs -> (length xs, head xs)) packed

-- Problem 11 ~ 20 : Lists, continued --
data Enc a = Multiple Int a | Single a deriving Show

-- 13
encodeModified :: (Eq a) => [a] -> [Enc a]
encodeModified = map f . encode
                       where f (1, x) = Single x
                             f (n, x) = Multiple n x

-- 12
decodeModified :: [Enc a] -> [a]
decodeModified = concatMap f
                    where f (Single a) = [a]
                          f (Multiple n a) = replicate n a

-- 13
encodeDirect :: (Eq a) => [a] -> [Enc a]
encodeDirect [] = []
encodeDirect (x:xs) = toInc n : encodeDirect remainder
                          where (n,remainder) = count xs 1
                                toInc 1 = Single x
                                toInc n = Multiple n x
                                count [] n = (n, [])
                                count all@(a:as) n = if a == x then count as (n+1) else (n, all)

-- 14
dupli :: [a] -> [a]
dupli = concatMap f
        where f a = [a,a]
-- 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- 16
partition n [] = []
partition n xs = let (h,t) = splitAt n xs
                 in h : partition n t

dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = concatMap (take (n - 1)) $ partition n xs

-- 17
split :: [a] -> Int -> ([a],[a])
split [] n = ([], [])
split xs n = (take n xs, drop n xs)

-- 18
slice [] n m = []
slice xs n m = drop (n-1) $ take m xs

-- 19
rotate :: [a] -> Int -> [a]
rotate xs n = drop n' xs ++ take n' xs
              where n' = mod n (length xs)

-- 20
removeAt :: [a] -> Int -> (a, [a])
removeAt (x:xs) 1 = (x, xs)
removeAt (x:xs) n = let (a,as) = removeAt xs (n-1)
                    in (a, x:as)

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

-- 24
diff_select :: Int -> Int -> IO [Int]
diff_select n m = rndSelect [1..m] n

-- 25
rnd_permu = shuffle

-- 26
combinations :: Int -> [a] -> [[a]]
combinations n = filter (\x -> length x == n) . subsequences
-- or
-- combinations 0 xs = [[]]
-- combinations n [] = []
-- combinations n (x:xs) = (map (x:) $ combinations (n-1) xs) ++
--                         combinations n xs

-- Problem 31 ~ 41 : Arithmetic --
-- Problem 46 ~ 50 : Logic and codes --
-- Problem 54A ~ 60 : Binary trees --
-- Problem 61 ~ 69 : Binary trees, continued --
-- Problem 70B ~ 73 : Multiway trees --
-- Problem 80 ~ 89 : Graphs --
-- Problem 90 ~ 94 : Miscellaneous problems --
-- Problem 95 ~ 99 : Miscellaneous problems, continued --
