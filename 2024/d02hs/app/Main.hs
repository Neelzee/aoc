module Main where

import Data.List (find, findIndex, partition, sort)
import Data.List.NonEmpty (unfold)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type Report = [Int]
type PuzzleInput = [Report]

main :: IO ()
main = do
  inpt <- getInput
  let valid = filter validate inpt
  print $ length valid
  let (super_valid, second_chance) = partition validate inpt
  let second_chance_valid = filter problemHampener second_chance
  print $ length super_valid + length second_chance_valid

validate :: Report -> Bool
validate x = validateDiff x && validateIncrDecr x

getInput :: IO PuzzleInput
getInput = do
  dt <- readFile "input.txt"
  return $ map (mapMaybe (\x -> readMaybe x :: Maybe Int) . words) $ lines dt

validateDiff :: [Int] -> Bool
validateDiff (x : y : xs) = minDiff <= diff && diff <= maxDiff && validateDiff (y : xs)
 where
  minDiff = 1
  maxDiff = 3
  diff = abs (x - y)
validateDiff _ = True

validateIncrDecr :: Report -> Bool
validateIncrDecr xs = ys == xs || reverse ys == xs
 where
  ys = sort xs

groups :: Report -> [(Int, Int)]
groups (x : y : xs) = (x, y) : groups (y : xs)
groups _ = []

spourp :: [(Int, Int)] -> Report
spourp [] = []
spourp ((x, _) : xs) = x : map fst (init xs) ++ [fst $ last xs, snd $ last xs]

problemHampener :: Report -> Bool
problemHampener xs = diffOk && (incrOk || decrOk)
 where
  ys = groups xs
  diffOk = case findIndex (not . validateDiffElem) ys of
    Just i -> validate $ spourp $ take i ys ++ drop (i + 1) ys
    Nothing -> True
  incrOk = case findIndex (not . validateIncrElem) ys of
    Just i -> validate $ spourp $ take i ys ++ drop (i + 1) ys
    Nothing -> True
  decrOk = case findIndex (not . validateDecrElem) ys of
    Just i -> validate $ spourp $ take i ys ++ drop (i + 1) ys
    Nothing -> True

validateElem :: (Int, Int) -> Bool
validateElem x = validateDiffElem x && (validateIncrElem x || validateDecrElem x)

validateIncrElem :: (Int, Int) -> Bool
validateIncrElem (x, y) = x > y

validateDecrElem :: (Int, Int) -> Bool
validateDecrElem (x, y) = x < y

validateDiffElem :: (Int, Int) -> Bool
validateDiffElem (x, y) = minDiff <= diff && diff <= maxDiff
 where
  minDiff = 1
  maxDiff = 3
  diff = abs (x - y)
