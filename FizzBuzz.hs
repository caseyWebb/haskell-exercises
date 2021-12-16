fizzorbuzz :: Int -> String
fizzorbuzz n
  | divBy 15 = "FizzBuzz"
  | divBy 5 = "Buzz"
  | divBy 3 = "Fizz"
  | otherwise = show n
  where
    divBy y = n `mod` y == 0

fizzbuzz :: [String]
fizzbuzz = [fizzorbuzz i | i <- [1 .. 100]]

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum xs

compare' :: (Ord a) => a -> a -> Ordering
compare' x y
  | x > y = GT
  | x < y = LT
  | otherwise = EQ