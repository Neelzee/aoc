{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import System.IO ( hGetContents, IOMode(ReadMode), openFile )
import Control.Monad ()
import Rule

main :: IO ()
main = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  let rules = parse contents
  let trees = map toTree rules
  let gp = head $ filter (\x -> name x == shinyGold) trees
  print trees
  print gp
  print $ sumTree trees gp

contain :: String
contain = "contain"

shinyGold :: String
shinyGold = "shiny gold"

parse :: String -> [Rule]
parse "" = []
parse xs = map rule $ lines xs

rule :: String -> Rule
rule xs = Rule (getColour xs) (getChildren xs)

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

data Tree =
  Tree { name :: String
       , kids :: [String]
       } deriving (Show)


instance Eq Tree where
  (==) (Tree l _) (Tree r _) = l == r


toTree :: Rule -> Tree
toTree (Rule nm kd) = Tree nm kd

dfs :: String -> [Tree] -> Tree -> Bool
dfs target trees = dfsHelper []
  where
    dfsHelper :: [Tree] -> Tree -> Bool
    dfsHelper checked t@(Tree i xs) = let
        newChecked = t : checked
      in
        i == target || any (dfsHelper newChecked) (filter (\(Tree nm _) -> nm `elem` xs) trees)

sumTree :: [Tree] -> Tree -> Int
sumTree trees = sumTreeHelper
  where
    sumTreeHelper :: Tree -> Int
    sumTreeHelper t@(Tree _ xs) = let
        transform :: String -> Tree
        transform x = find (\(Tree nm _) -> nm == x) trees
        filteredYs :: [Tree]
        filteredYs = map transform xs
      in
        length xs + sum (map sumTreeHelper filteredYs)

find :: (a -> Bool) -> [a] -> a
find _ [] = undefined
find f (x:xs)
  | f x = x
  | otherwise = find f xs