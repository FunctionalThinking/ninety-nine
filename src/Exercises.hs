module Exercises where

import Control.Monad
import Data.Array.IO
import System.Random (randomRIO)
import Data.List (group, subsequences, (\\), sortOn)
import Data.Char (isLetter)
import Data.Maybe
import Control.Arrow ((&&&))
import Control.Applicative
import Text.ParserCombinators.ReadP

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
--
huffman :: [(Char, Int)] -> [(Char, String)]
huffman freqs = let tree = hTree freqs
                in sortOn fst $ hFlatten tree

hFlatten :: HTree -> [(Char, String)]
hFlatten = hFlatten' ""

hFlatten' :: String -> HTree -> [(Char, String)]
hFlatten' path (HLeaf (c,_)) = [(c, reverse path)]
hFlatten' path (HBranch left _ right) = hFlatten' ('0':path) left
                                    ++ hFlatten' ('1':path) right

data HTree = HLeaf (Char,Int) | HBranch HTree Int HTree deriving (Show)

hTree :: [(Char, Int)] -> HTree
hTree freqs = head $ until singleton mergeTwo (map HLeaf freqs)

mergeTwo :: [HTree] -> [HTree]
mergeTwo treeList =  let t1:t2:rest = sortTrees treeList
                     in merge t1 t2:rest

merge :: HTree -> HTree -> HTree
merge t1 t2 = let v1 = hValue t1
                  v2 = hValue t2
              in HBranch t1 (v1 + v2) t2

sortTrees :: [HTree] -> [HTree]
sortTrees = sortOn hValue

hValue :: HTree -> Int
hValue (HLeaf (_,n)) = n
hValue (HBranch _ n _) = n

singleton :: [a] -> Bool
singleton [_] = True
singleton _ = False

-- Problem 54A ~ 60 : Binary trees --
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- 55 completely balanced binary tree
cbalTree :: Int -> [Tree Char]
cbalTree n
  | n == 0 = [Empty]
  | odd n = Branch 'x' <$> cbalTree (n `div` 2) <*> cbalTree (n `div` 2)
  | otherwise = Branch 'x' <$> cbalTree (n `div` 2) <*> cbalTree ((n `div` 2) - 1)
                <|> Branch 'x' <$> cbalTree ((n `div` 2) - 1) <*> cbalTree (n `div` 2)

-- 56
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ a b) = mirror a b

mirror :: Tree a -> Tree b -> Bool
mirror Empty Empty = True
mirror (Branch _ left1 right1) (Branch _ left2 right2) = mirror left1 right2 && mirror right1 left2
mirror _ _ = False

-- 57 Binary Search tree
construct :: (Ord a) => [a] -> Tree a
construct = foldl insert Empty

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty a = Branch a Empty Empty
insert (tree@(Branch root left right)) a
  | a < root = Branch root (insert left a) right
  | a > root = Branch root left (insert right a)
  | otherwise = tree

-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

-- 59 Height Balanced Binary Trees
{-
> take 4 $ hbalTree 'x' 3
[Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]

    x                 x                  x                  x
  x   x             x   x              x   x              x   x
        x              x                  x x              x
-}

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree a 1 = [Branch a Empty Empty]
hbalTree a n =  Branch a <$> n2 <*> n1
            <|> Branch a <$> n1 <*> n2
            <|> Branch a <$> n1 <*> n1
            where n2 = hbalTree a (n-2)
                  n1 = hbalTree a (n-1)

-- 60 Height Balanced Binary Trees with a Given Number of Nodes
{-
> length $ hbalTreeNodes 'x' 15
1553
-}
minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes h = minNodes (h-1) + minNodes (h-2) + 1

maxHeight :: Int -> Int
maxHeight n = height - 1
              where height = length base
                    base = takeWhile (<= n) $ map minNodes [0..]


minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 (fromIntegral (n + 1))

minMaxHeight :: Int -> (Int, Int)
minMaxHeight n = (minHeight n , maxHeight n)

validBalance :: Int -> Int -> Bool
validBalance left right = min maxL maxR - max minL minR >= -1
    where (minL, maxL) = minMaxHeight left
          (minR, maxR) = minMaxHeight right


hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes a 1 = [Branch a Empty Empty]
hbalTreeNodes a n = filter hBalance
                    [Branch a left right| leftN <- [0..n-1]
                      , let rightN = n - 1 - leftN
                      , validBalance leftN rightN
                      , left <- hbalTreeNodes a leftN
                      , right <- hbalTreeNodes a rightN]

hBalance Empty = True
hBalance (Branch _ left right) = hBalance left && hBalance right && abs(leftH - rightH) <= 1
  where leftH = height left
        rightH = height right

height Empty = 0
height (Branch _ left right) = 1 + max (height left) (height right)

-- Problem 61 ~ 69 : Binary trees, continued --

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

-- 61 count leaves
-- > countLeaves tree4
-- 2
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + countLeaves right

-- 61A collect leaves
-- > leaves tree4
-- [4,2]
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch _ left right) = leaves left ++ leaves right

-- 62 Collect the internal nodes of a binary tree in a list
-- Prelude>internals tree4
-- Prelude>[1,2]
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch a left right) = a : internals left ++ internals right

-- 62B
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch _ left right) n = atLevel left (n-1) ++ atLevel right (n-1)

-- 63 Construct a complete binary tree
-- A complete binary tree with height H is defined as follows:
-- The levels 1,2,3,...,H-1 contain the maxim
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = go 1
  where
    go index = if index > 0 &&  index <= n then Branch 'X' (go (index*2)) (go (index*2 + 1)) else Empty


tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

-- 64
layout :: Tree a -> Tree (a, (Int, Int))
layout t = fst $ sublayout t 1 0
           where sublayout Empty h c = (Empty, 0)
                 sublayout (Branch a left right) h c = (Branch (a, (c+leftcount+1, h)) lefttree righttree, leftcount + rightcount + 1)
                                                     where (lefttree, leftcount) = sublayout left (h+1) c
                                                           (righttree, rightcount) = sublayout right (h+1) (c+leftcount+1)


-- 65 layout tree
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

layout65 :: Tree a -> Tree (a, (Int, Int))
layout65 Empty = Empty
layout65 tree = fitLeft tree'
  where h = height tree
        tree' = sublayout tree 0 1
        sublayout :: Tree a -> Int -> Int -> Tree (a, (Int, Int))
        sublayout Empty _ _ = Empty
        sublayout (Branch a left right) x y = Branch (a, (x,y)) (sublayout left (x-armlength) (y+1)) (sublayout right (x+armlength) (y+1))
          where armlength = 2 ^ (h - y - 1)

translateX :: Int -> Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
translateX _ Empty = Empty
translateX x (Branch (a,(x0,y)) left right) = Branch (a,(x0+x,y)) (translateX x left) (translateX x right)

leftmost :: Tree a -> a
leftmost (Branch a Empty _) = a
leftmost (Branch _ left _) = leftmost left
leftmost Empty = error "No leftmost"

fitLeft :: Tree (a,(Int,Int)) -> Tree (a,(Int,Int))
fitLeft tree = translateX offset tree
  where offset = let (_, (x,_)) = leftmost tree in (1 - x)

layout66:: Tree a -> Tree (a, (Int, Int))
layout66 tree = fitLeft $ layoutTree $ makeSymmetric $ compactLayout tree

compactLayout :: Tree a -> Tree (a, Int)
compactLayout Empty = Empty
compactLayout (Branch a left right) = Branch (a, armLength) left' right'
  where left' = compactLayout left
        right' = compactLayout right
        minDiff = minimum $ 0:zipWith (-) (getMinXs (layoutTree right')) (getMaxXs (layoutTree left'))     -- (0,1)
        armLength = head $ filter (\l -> l * 2 + minDiff > 0) [1..]
        getMinXs :: Tree (a, (Int, Int)) -> [Int]
        getMinXs tree = map (minimum . map getX) $ levels tree
        getMaxXs :: Tree (a, (Int, Int)) -> [Int]
        getMaxXs tree = map (maximum . map getX) $ levels tree
        getX (_, (x, _)) = x

makeSymmetric :: Tree (a, Int) -> Tree (a, Int)
makeSymmetric tree = setArmLengths tree maxArmLengths
  where maxArmLengths = map (maximum . map getArmLength) $ levels tree
        setArmLengths Empty _ = Empty
        setArmLengths (Branch (a,_) left right) (armlength:rest) =
            Branch (a,armlength) (setArmLengths left rest) (setArmLengths right rest)
        setArmLengths branch [] = branch
        getArmLength (_, armlength) = armlength

layoutTree :: Tree (a, Int) -> Tree (a, (Int, Int))
layoutTree tree = sub tree (0,1)
    where sub Empty _ = Empty
          sub (Branch (a, armlength) left right) (x,y) =
            Branch (a,(x,y)) (sub left (x-armlength,y+1)) (sub right (x+armlength,y+1))

levels :: Tree a -> [[a]]
levels Empty = []
levels (Branch a left right) = [a] : zipAppend (levels left) (levels right)

zipAppend :: Monoid m => [m] -> [m] -> [m]
zipAppend (a:as) (b:bs) = mappend a b : zipAppend as bs
zipAppend (a:as) [] = map (`mappend` mempty) (a:as)
zipAppend [] (b:bs) = map (`mappend` mempty) (b:bs)
zipAppend [] [] = []

-- 67A
-- "x(y,a(,b))" --> Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

stringToTree:: String -> Maybe (Tree Char)
stringToTree input = listToMaybe [tree | (tree, "") <- readP_to_S treeP input]

treeP :: ReadP (Tree Char)
treeP = Branch <$> letterP <* char '(' <*> treeP <* char ',' <*> treeP <* char ')'
  <|> Branch <$> letterP <*> return Empty <*> return Empty
  <|> return Empty

letterP :: ReadP Char
letterP = satisfy isLetter

treeToString:: Tree Char -> String
treeToString tree = treeToString' tree ""
treeToString' Empty = id
treeToString' (Branch a Empty Empty) = showChar a
treeToString' (Branch a left right) = do
  showChar a
  showChar '('
  treeToString' left
  showChar ','
  treeToString' right
  showChar ')'

-- Problem 70B ~ 73 : Multiway trees --
-- Problem 80 ~ 89 : Graphs --
-- Problem 90 ~ 94 : Miscellaneous problems --
-- Problem 95 ~ 99 : Miscellaneous problems, continued --
