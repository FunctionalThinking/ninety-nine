module Graph where

data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)
data Adj a = Adj [(a, [a])] deriving (Show, Eq)


-- 80 (***) Conversions
-- Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
-- Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
graphToAdj :: Eq a => Graph a -> Adj a
graphToAdj (Graph nodes edges) = Adj adjs
  where adjs = zip nodes $ map neighbors nodes
        neighbors n = concatMap (neighbor n) edges
        neighbor n (a,b) 
          | n == a = [b]
          | n == b = [a]
          | otherwise = []

