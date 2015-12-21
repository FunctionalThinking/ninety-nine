module MultiwayTree where

import Control.Monad.State

data Tree a = Node a [Tree a] deriving (Eq, Show)

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
    Node 'f' [Node 'g' []],
    Node 'c' [],
    Node 'b' [Node 'd' [], Node 'e' []]
  ]

-- 70c Count the nodes of a multiway tree.

nnodes :: Tree a -> Int
nnodes (Node _ ts) = 1 + sum (map nnodes ts)

-- 70 Tree construction from a node string.

--  stringToTree "afg^^c^bd^e^^^"
stringToTree :: String -> Tree Char
stringToTree input = let (t,"") = runState tree input in t
  where tree :: State String (Tree Char)
        tree = do
          n <- ch
          ts <- trees
          ch -- '^'
          return $ Node n ts 
        ch :: State String Char
        ch = do
          (c:cs) <- get
          put cs
          return c
        trees :: State String [Tree Char]
        trees = do
          (c:cs) <- get
          if c == '^' then 
            return []
          else
            do {t <- tree; ts <- trees; return (t:ts)}

treeToString :: Tree Char -> String
treeToString (Node a ts) = [a] ++ concatMap treeToString ts ++ "^"

-- 71 Determine the internal path length of a tree.
ipl :: Tree a -> Int
ipl = ipl' 0
  where ipl' depth (Node _ ts) = depth + sum (map (ipl' (depth + 1)) ts)

-- 72 (*) Construct the bottom-up order sequence of the tree nodes.
bottom_up :: Tree Char -> String
bottom_up (Node a ts) = concatMap bottom_up ts ++ [a]

-- 73 (**) Lisp-like tree representation.
displayLisp :: Tree Char -> String
displayLisp (Node a ts) = 
  if ts == [] then [a]
  else "(" ++ unwords ([a]:map displayLisp ts) ++ ")"
