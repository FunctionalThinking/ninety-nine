module Exercises where

import Control.Monad
import Data.Array.IO
import System.Random (randomRIO)
import Data.List (group, subsequences, (\\), sortOn)
import Control.Arrow ((&&&))

-- Problem 1 ~ 10 : Lists --

-- 1
myLast :: [a] -> a
myLast [] = undefined
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = undefined
myButLast [x1,_] = x1
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt (a:_) 0 = a
elementAt [] _ = undefined
elementAt (_:as) n = elementAt as (n-1)

-- 4
myLength :: [a] -> Int
myLength = foldr (\_ x -> x + 1) 0

-- 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 6
isPalindrome:: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs

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
            in map (length &&& head) packed

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
                          where (n, remainder) = count xs 1
                                toInc 1 = Single x
                                toInc n' = Multiple n' x
                                count [] n' = (n', [])
                                count t@(a:as) n' = if a == x then count as (n'+1) else (n, t)

-- 14
dupli :: [a] -> [a]
dupli = concatMap f
        where f a = [a,a]
-- 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- 16
partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = let (h,t) = splitAt n xs
                 in h : partition n t

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = concatMap (take (n - 1)) $ partition n xs

-- 17
split :: [a] -> Int -> ([a],[a])
split [] _ = ([], [])
split xs n = (take n xs, drop n xs)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
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
removeAt _ _ = undefined

-- Problem 21 ~ 28 : Lists again --
-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt a' as     1         = a':as
insertAt a' []     n         = if n == 1 then  [a']
                               else error "Invalid position"
insertAt a' (a:as) n         = a:insertAt a' as (n-1)

-- 22
range :: (Enum a) => a -> a -> [a]
range = enumFromTo

-- 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect as n = take n <$> shuffle as

shuffle :: [a] -> IO [a]
shuffle xs = do
                ar <- newArray' n xs
                forM [1..n] $ \i -> do
                    j <- randomRIO (i,n)
                    vi <- readArray ar i
                    vj <- readArray ar j
                    writeArray ar j vi
                    return vj
            where
              n = length xs
              newArray' :: Int -> [a] -> IO (IOArray Int a)
              newArray' n' =  newListArray (1, n')

-- 24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n

-- 25
rndPermu :: [a] -> IO [a]
rndPermu = shuffle

-- 26
combinations :: Int -> [a] -> [[a]]
combinations n = filter (\x -> length x == n) . subsequences
-- or
-- combinations 0 xs = [[]]
-- combinations n [] = []
-- combinations n (x:xs) = (map (x:) $ combinations (n-1) xs) ++
--                         combinations n xs


-- 27
groupN :: Eq a => [Int] -> [a] -> [[[a]]]
groupN [] _     = [[]]
groupN (n:ns) as = [ combi : sub |
                       combi <- combinations n as ,
                       let rest = as \\ combi,
                       sub <- groupN ns rest]
-- 28
--a
lsort :: [[a]] -> [[a]]
lsort  = sortOn length

freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (== x) xs

--b sort by frequence of length
lfsort :: [[a]] -> [[a]]
lfsort xss = sortOn lengthFreq xss
             where lengthFreq xs = freq (length xs) lengths
                   lengths = map length xss

-- Problem 31 ~ 41 : Arithmetic --
-- 31
isPrime:: Int -> Bool
isPrime n = [1,n] == divisors
            where divisors =
                   filter (\d -> n `mod` d == 0) [1..n]

--32
myGCD :: Int -> Int -> Int
myGCD a b
  | a == 0    = b
  | a < b     = myGCD (b `mod` a) a
  | otherwise = myGCD (a `mod` b) b

-- 33
coprime :: Int -> Int -> Bool
coprime a b = 1 == myGCD a b

-- 34
totient :: Int -> Int
totient m = length $ filter (coprime m) [1..m]

-- 35
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = f: primeFactors (n `quot` f)
                 where f = head $ filter (\d -> n `mod` d == 0) [2..]

-- 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map swap $ encode $ primeFactors n
                     where swap (a, b) = (b, a)

-- 37
totient' :: Int -> Int
totient' = product . map f . primeFactorsMult
             where f (p, m) = (p - 1) * p ^ (m - 1)

-- 38
{-
*Exercises> :set +s
*Exercises> totient 10090
4032
(0.08 secs, 0 bytes)
*Exercises> totient' 10090
4032
(0.00 secs, 0 bytes)
-}

-- 39
primeR :: Int -> Int -> [Int]
primeR from to = filter isPrime [from .. to]

-- 40
goldbach :: Int -> (Int, Int)
goldbach n
     | odd n = error "Odd number!"
     | otherwise = head $ filter (\(_, p') -> isPrime p') $ map (\p -> (p, n-p)) $ filter isPrime [1..n]

-- 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b =
  [goldbach n | n <- [(max 3 a)..b], even n]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b lower = filter (\(l, u)-> l>lower && u>lower) $ goldbachList a b

-- Problem 46 ~ 50 : Logic and codes --

-- 46
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) (.) (.)

infixl 3 `and'`
infixl 2 `or'`
infixl 1 `equ'`

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' = not .: and'

nor' :: Bool -> Bool -> Bool
nor' = not .: or'

xor' :: Bool -> Bool -> Bool
xor' = (/=)

impl' :: Bool -> Bool -> Bool
impl' a b = not a || b

equ' :: Bool -> Bool -> Bool
equ' = (==)

table' :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table' f = [(a, b, f a b) | a <- [True,False], b <- [True, False]]

table :: (Bool -> Bool -> Bool) -> IO()
table f = putStrLn $ unlines $ map format (table' f)
          where format (a,b,r) = unwords $ map show [a, b, r]

-- 48
makeInput :: Int -> [[Bool]]
makeInput 0 = [[]]
makeInput n = [ b:r | b <- [True,False], r <- rest]
              where rest = makeInput (n-1)
tableN':: Int -> ([Bool] -> Bool) -> [([Bool], Bool)]
tableN' n f = [(input, f input)| input <- makeInput n]

tableN :: Int -> ([Bool] -> Bool) -> IO()
tableN n f = putStrLn $ unlines $ map format $ tableN' n f
             where format (input,result) = unwords $ map show $ input ++ [result]

-- 49
gray :: Int -> [String]
gray 0 = [[]]
gray n = [h:t | h <-"01", t <- rest]
         where rest = gray (n-1)

-- 50

huffman :: [(Char, Int)] -> [(Char, String)]
huffman freqs =  [ hc c | (c,_) <- freqs ]
                  where hc c = undefined
                        tree = until singleton mergeTwo initTree

-- Problem 54A ~ 60 : Binary trees --
-- Problem 61 ~ 69 : Binary trees, continued --
-- Problem 70B ~ 73 : Multiway trees --
-- Problem 80 ~ 89 : Graphs --
-- Problem 90 ~ 94 : Miscellaneous problems --
-- Problem 95 ~ 99 : Miscellaneous problems, continued --
