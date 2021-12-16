module HelloWorld where

import Data.Char

hw :: IO ()
hw = putStrLn "Hello World"

greet :: IO ()
greet = do
  putStrLn "What is you name?"
  name <- getLine
  let uname = map toUpper name
  putStrLn ("Hello " ++ uname ++ "!")

main :: IO ()
main = do
  i <- getLine
  if i /= "quit"
    then do
      putStrLn ("Input: " ++ i)
      main
    else return ()

count :: Int -> Int -> IO ()
count n m = do
  putStrLn (show n)
  if n < m
    then count (n + 1) m
    else return ()