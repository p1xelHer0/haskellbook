module BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Nothing -> Leaf
    Just (a, b, c) -> Node (unfold f a) b (unfold f c)

-- 2. Make a tree builder
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = takeTree n $ unfold (\a -> Just (a + 1, a, a + 1)) 0

takeTree :: Integer -> BinaryTree a -> BinaryTree a
takeTree 0 _ = Leaf
takeTree _ Leaf = Leaf
takeTree n (Node left a right) =
  Node (takeTree (n - 1) left) a (takeTree (n - 1) right)

testTree =
  Node
    (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))
    0
    (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))
