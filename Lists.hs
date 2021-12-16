module Lists where

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x : xs) = max' x (maximum' xs)

replicate' :: (Ord b, Num b) => a -> b -> [a]
replicate' a 0 = []
replicate' a n = a : replicate' a (n - 1)

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n _
  | n <= 0 = []
take' _ [] = []
take' i (x : xs) = x : take' (i - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs) = (a == x) || elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let left = [a | a <- xs, a <= x]
      right = [a | a <- xs, a > x]
   in left ++ [x] ++ right

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (without x xs)

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x : xs)
  | x `elem` xs = nub' xs
  | otherwise = x : nub' xs

without :: (Eq a) => a -> [a] -> [a]
without _ [] = []
without a (x : xs)
  | a == x = without a xs
  | otherwise = a : without a xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x : xs) = x <= head xs && isAsc xs

any' :: (Eq a) => (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' pred (x : xs) = pred x || any' pred xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (filter f xs, filter (not . f) xs)

graph :: [(Int, Int)]
graph = [(1, 2), (2, 3), (3, 2), (4, 3), (4, 5)]

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath xs a b =
  let (possibleStartingPoints, rest) = partition (\(a', _) -> a' == a) xs
   in any (\(a', b') -> a' == b || b' == b || hasPath rest b' b) possibleStartingPoints

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x :) acc) []