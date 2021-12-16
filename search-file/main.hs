module Main where

import Data.Char (toLower)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "Specify the words to search:"
  wordsToSearch <- getWordsToSearch

  putStrLn "File to search:"
  fileName <- getLine
  fileContents <- readFile fileName

  putStr $ formatResults $ search fileContents wordsToSearch

  return ()

getWordsToSearch :: IO [String]
getWordsToSearch = do
  putStr "> "
  word <- getLine
  if word == ""
    then return []
    else do
      rest <- getWordsToSearch
      return (word : rest)

contains :: String -> String -> Bool
contains [] _ = True
contains _ [] = False
contains (x : xs) (y : ys)
  | toLower x == toLower y = contains xs $ take (length xs) ys
  | otherwise = contains (x : xs) ys

search :: String -> [String] -> ([String], [String])
search fileContents =
  foldr
    ( \word (found, notFound) ->
        if contains word fileContents
          then (word : found, notFound)
          else (found, word : notFound)
    )
    ([], [])

formatResults :: ([String], [String]) -> String
formatResults ([], []) = ""
formatResults (x : xs, notFound) = "\"" ++ x ++ "\" found\n" ++ formatResults (xs, notFound)
formatResults ([], x : xs) = "\"" ++ x ++ "\" NOT found\n" ++ formatResults ([], xs)