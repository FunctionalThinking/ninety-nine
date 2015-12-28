module Graph where

import Prelude hiding (cycle)

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

-- 81 (**) Path from one node to another one
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


-- 82 (*) Cycle from a given node
cycle :: Eq a => a -> [(a,a)] -> [[a]]
cycle s edges = map (s:) $ concatMap (\n' -> paths n' s edges) $ dneighbors s edges


-- 83 (**) Construct all spanning trees
spanTree :: Graph a -> [Graph a]
spanTree a
  | not (isConnected a) = []
  | isTree a = [a]
  | otherwise =  concatMap spanTree subgraphs
    where subgraphs = undefined
