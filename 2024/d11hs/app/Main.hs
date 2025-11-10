module Main where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
  str <- readFile "input.txt"
  let rawStones = words str
  let stones = mapMaybe (\c -> readMaybe c :: Maybe Int) rawStones
  let newStones = concatMap (getStone 5) stones
  print newStones
  print $ length newStones

getStone :: Int -> Int -> [Int]
getStone = s
 where
  s 0 n = [n]
  s i 0 = s (i - 1) 1
  s i n
    | even len = concatMap (s (i - 1)) [first, lst]
    | otherwise = [n * 2024]
   where
    digits = digs n
    len = length digits
    first = undigs $ take (len `div` 2) digits
    lst = undigs $ drop (len `div` 2) digits

digs :: Int -> [Int]
digs n
  | n <= 9 = [n]
  | otherwise = n `mod` 10 : digs (n `div` 10)

undigs :: [Int] -> Int
undigs [] = 0
undigs (x : xs) = x + (undigs xs * 10)
