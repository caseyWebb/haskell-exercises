module BinarySearchTree where

import Data.List hiding (elem, insert)
import Test.QuickCheck (quickCheck)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Read, Eq)

instance Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Node a left right) = foldMap f left <> f a <> foldMap f right

  foldr f acc Leaf = acc
  foldr f acc (Node a left right) = foldr f (f a $foldr f acc right) left

instance (Show a) => Show (Tree a) where
  show Leaf = ""
  show (Node a left right) = show left ++ show a ++ show right

-- singleton :: a -> Tree a
-- singleton x = Node x Leaf Leaf

insert :: (Ord a) => a -> Tree a -> Tree a
insert v Leaf = Node v Leaf Leaf
insert v (Node a left right)
  | v <= a = Node a (insert v left) right
  | otherwise = Node a left (insert v right)

insertMany :: (Ord a) => [a] -> Tree a -> Tree a
insertMany xs tree = foldl (flip insert) Leaf xs

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node a left right) = inorder left ++ a : inorder right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ Leaf = False
treeElem x (Node a left right)
  | x == a = True
  | otherwise = treeElem x $ if x < a then left else right

prop :: (Ord a) => [a] -> Bool
prop xs = sort xs == inorder (insertMany xs Leaf)