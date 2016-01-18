module Graph where

import Prelude hiding (cycle)
import Data.List (nub, sortOn, (\\))

data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)
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
spanTree :: (Eq a) => Graph a -> [Graph a]
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
