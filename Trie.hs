module Trie where

data Trie a = Leaf a | Node a [Trie a]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf a) = f acc a
foldtrie f acc (Node a tries) = foldl (foldtrie f) (f acc a) tries