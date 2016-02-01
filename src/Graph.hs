module Graph where

import Prelude hiding (cycle)
import Data.List (nub, sort, sortOn, (\\), permutations)
import Debug.Trace
data Graph a = Graph [a] [(a,a)] deriving (Show)

instance Functor Graph where
  fmap f (Graph nodes edges) = Graph (fmap f nodes) [(f a, f b) | (a,b) <- edges]

instance (Ord a) => Eq (Graph a) where
  (Graph nodesA edgesA) == (Graph nodesB edgesB) = sort nodesA == sort nodesB && sort edgesA == sort edgesB


data Adj a = Adj [(a, [a])] deriving (Show, Eq)


-- 80 (***) Conversions
-- Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
-- Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
graphToAdj :: Eq a => Graph a -> Adj a
graphToAdj (Graph nodes edges) = Adj adjs
  where adjs = zip nodes $ map (`neighbors` edges) nodes

neighbors :: Eq a => a -> [(a,a)] -> [a]
neighbors n = concatMap (neighbor n)
  where
    neighbor n' (a,b)
            | n' == a = [b]
            | n' == b = [a]
            | otherwise = []

-- 81 (**) Path from one node to another one (for directed)
paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths start end graph
  | start == end = [[start]]
  | otherwise =  map (start:) subsolution
    where subsolution = concatMap (\n -> paths n end subgraph) (dneighbors start graph)
          subgraph = ddeleteNode start graph

dneighbors :: Eq a => a -> [(a,a)] -> [a]
dneighbors n = map snd . filter ((==n).fst)

ddeleteNode :: Eq a => a -> [(a,a)] -> [(a,a)]
ddeleteNode a = filter ((/=a).fst)


-- 82 (*) Cycle from a given node (for directed!)
cycle :: Eq a => a -> [(a,a)] -> [[a]]
cycle s edges = map (s:) $ concatMap (\n' -> paths n' s edges) $ dneighbors s edges

k4 :: Graph Char
k4 = Graph "abcd" [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

-- 83 (**) Construct all spanning trees
spanTree :: (Ord a) => Graph a -> [Graph a]
spanTree = nub . spanTree'
spanTree' g
  | not (isConnected g) = []
  | isTree g = [g]
  | otherwise =  concatMap spanTree $ subgraphs g

subgraphs :: Eq a => Graph a -> [Graph a]
subgraphs (Graph nodes edges) = [Graph nodes edges' | e <- edges, let edges' = filter (/= e) edges]

isTree :: Eq a => Graph a -> Bool
isTree (Graph nodes edges) = and [1 == length (paths' a b edges) | a <- nodes, b <- nodes, a /= b]

paths' :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths' start end edges
  | start == end = [[start]]
  | otherwise =  map (start:) subsolution
    where subsolution = concatMap (\n -> paths' n end subgraph) (neighbors start edges)
          subgraph = deleteNode start edges

deleteNode :: Eq a => a -> [(a,a)] -> [(a,a)]
deleteNode a = filter f
  where f (b,c) = a /= b && a /= c

isConnected :: Eq a => Graph a -> Bool
isConnected (Graph [] _) = True
isConnected (Graph (n:ns) edges) = all hasPath ns
  where
    hasPath to = paths' n to edges /= []


-- 84
prim :: (Eq a, Ord w) => [a] -> [(a,a,w)] -> [(a,a,w)]
prim nodes@(n:ns) edges = go [n] [] neighborEdges (edges \\ neighborEdges)
  where go connected treeedges neighbors unseen =
          if length nodes == length connected
          then treeedges
          else go connected' treeedges' neighbors' unseen'
          where
            e@(a,b,_):rest = sortOn weight neighbors
            added = if a `elem` connected then b else a
            connected' = added : connected
            treeedges' = e : treeedges
            neighbors' = filter f (edgesOf added ++ rest)
              where f (a,b,_) = not (a `elem` connected' && b `elem` connected')
            edgesOf n = filter (on n) unseen
            unseen' = filter (not.on n) unseen
        neighborEdges= filter (on n) edges
        on node (a,b,_) = node == a || node == b
        weight (_,_,w) = w


-- 85
{-
(**) Graph isomorphism

Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic
if there is a bijection
  f: N1 -> N2 such that
  for any nodes X,Y of N1, X and Y are adjacent if and only if f(X) and f(Y) are adjacent.

Write a predicate that determines whether two graphs are isomorphic.
Hint: Use an open-ended list to represent the function f.

Example in Haskell:

graphG1 = [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
iso graphG1 graphH1
True
-}

graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

iso :: (Eq a, Ord b) => Graph a -> Graph b -> Bool
iso a@(Graph nodesA edgesA) b@(Graph nodesB edgesB) = any (\f -> fmap f a == b) fs
  where fs = [ makeF nodesA nodesB' | nodesB' <- permutations nodesB]
        makeF as bs = \a -> snd $ head $ filter (\(a',_) -> a' == a) pairs
          where pairs = zip as bs


-- 86
{-
8 Problem 86
(**) Node degree and graph coloration

a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a given node.

b) Write a predicate that generates a list of all nodes of a graph sorted according to decreasing degree.

c) Use Welsh-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have different colors.

Example in Haskell:
http://www.slideshare.net/PriyankJain26/graph-coloring-48222920

kcolor (Graph ['a','b','c','d','e','f','g','h','i','j']
[('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),
 ('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),
 ('f','h'),('f','i'),('g','i'),('g','j'),('h','j')])
[('a',1),('b',2),('c',1),('d',2),('e',3),('f',2),('g',1),('h',3),('i',3),('j',2)]
-}

kcolor :: (Show a, Eq a) => Graph a -> [(a,Int)]
kcolor (Graph nodes edges) = [ (n, color) | n <- nodes, let (Just color) = lookup n colorMap ]
  where colorMap = loop sortedNodes 1 [] [] []
        sortedNodes = map fst $ sortOn (negate.snd) [ (n, degree n) | n <- nodes]
        degree node = length $ filter (\(a,b) -> a == node || b == node) edges
        loop [] _ colored _ [] = colored
        loop [] color colored coloredNeighbors uncolored = colored ++ loop (reverse uncolored) (color+1) [] [] []
        loop (n:ns) color colored coloredNeighbors uncolored =
              if n `elem` coloredNeighbors
              then loop ns color colored coloredNeighbors (n:uncolored)
              else loop ns color ((n,color):colored) (neighbors n edges ++ coloredNeighbors) uncolored
