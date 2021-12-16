data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

-- A tree representing tuples of how many times left vs right was chosen
--           (0, 0)
--     (1, 0)       (0, 1)
-- (2, 0) (1, 1) (1, 1) (0, 2)
tree :: Tree (Integer, Integer)
tree = mknode (0, 0)
  where
    mknode (l, r) = Node (mknode (l + 1, r)) (l, r) (mknode (l, r + 1))

-- cut :: Integer -> Tree (Integer, Integer) -> Tree (Integer, Integer)
-- cut _ Leaf = Leaf
-- cut depth (Node lnode (l, r) rnode) =
--   if max l r == depth
--     then tree
--     else Node (cut depth lnode) (l, r) (cut depth rnode)

-- cut :: Integer -> Tree a -> Tree a
-- cut _ Leaf = Leaf
-- cut 0 tree = case tree of
--   Leaf -> Leaf
--   Node _ a _ -> Node Leaf a Leaf
-- cut depth tree = case tree of
--   Leaf -> Leaf
--   Node left a right ->
--     Node (cut (depth - 1) left) a (cut (depth - 1) right)

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf
cut _ Leaf = Leaf
cut n (Node l v r) = Node (cut (n - 1) l) v (cut (n - 1) r)
