{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Data.Maybe (mapMaybe)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

test :: String
test =
  "\
  \.......S.......\n\
  \...............\n\
  \.......^.......\n\
  \...............\n\
  \......^.^......\n\
  \...............\n\
  \.....^.^.^.....\n\
  \...............\n\
  \....^.^...^....\n\
  \...............\n\
  \...^.^...^.^...\n\
  \...............\n\
  \..^...^.....^..\n\
  \...............\n\
  \.^.^.^.^.^...^.\n\
  \...............\
  \"

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let xs = parseData contents
  print $ foo $ loop xs
  pure ()

data Cell
  = Start
  | Empty
  | Beam
  | Split Int
  deriving (Show, Eq)

parseCell :: Char -> Cell
parseCell 'S' = Start
parseCell '.' = Empty
parseCell '^' = Split 0
parseCell '|' = Beam

parseData :: String -> [[Cell]]
parseData xs = map (map parseCell) $ lines xs

getStart :: [[Cell]] -> (Int, Int)
getStart cs =
  head $
    map (\(x, y, _) -> (x, y)) $
      filter (\(_, _, c) -> c == Start) $
        mapMaybe (\(x, y) -> (x,y,) <$> get cs x y) $
          coords cs

getPos :: [[Cell]] -> [(Int, Int)]
getPos cs =
  map (\(x, y, _) -> (x, y)) $
    filter (\(_, _, c) -> c == Start || c == Beam) $
      mapMaybe (\(x, y) -> (x,y,) <$> get cs x y) $
        coords cs

coords :: [[a]] -> [(Int, Int)]
coords [z] = map (0,) [0 .. length z]
coords (z : zs) = concat $ [[(x, y) | y <- [0 .. length z]] | x <- [0 .. (length zs + 1)]]

get :: [[a]] -> Int -> Int -> Maybe a
get xs i j
  | i < 0 || j < 0 = Nothing
  | i >= length xs = Nothing
  | j >= length (xs !! i) = Nothing
  | otherwise = Just $ (xs !! i) !! j

put :: [a] -> Int -> a -> [a]
put [] 0 a = [a]
put [] i _ = error $ "Index out of bounds: " ++ show i
put (_ : xs) 0 x = x : xs
put (x : xs) n y = x : put xs (n - 1) y

put2 :: [[a]] -> Int -> Int -> a -> [[a]]
put2 [] 0 0 a = [[a]]
put2 [] i j _ = error $ "Index out of bounds: " ++ show i ++ " " ++ show j
put2 (x : xs) 0 j a = put x j a : xs
put2 (x : xs) i j a = x : put2 xs (i - 1) j a

iter :: [[Cell]] -> [(Int, Int)] -> [[Cell]]
iter cs ps = foldl updateCell cs ps
  where
    updateCell :: [[Cell]] -> (Int, Int) -> [[Cell]]
    updateCell vs (i, j) = case get cs i j of
      {- Start position should put a beam underneath itself,
      probablj no need to check its under position
      -}
      Just Start -> put2 vs (i + 1) j Beam
      -- If it is a beam, we check the position under
      Just Beam -> case get vs (i + 1) j of
        -- Replaces the empty spot with a Beam
        Just Empty -> put2 vs (i + 1) j Beam
        -- If its a split, check the positions, (i, j + 1), left and right side (i + 1, j ± 1)
        Just (Split n) -> case (get vs (i + 1) (j - 1), get vs (i + 1) (j + 1)) of
          -- Two empty places, so we increment twice
          (Just Empty, Just Empty) ->
            let leftVs = put2 vs (i + 1) (j - 1) Beam
                rightVs = put2 leftVs (i + 1) (j + 1) Beam
                splitVs = put2 rightVs (i + 1) j (Split (n + 2))
             in splitVs
          -- Just one, so increment once
          (_, Just Empty) -> put2 (put2 vs (i + 1) (j + 1) Beam) (i + 1) j (Split (n + 1))
          (Just Empty, _) -> put2 (put2 vs (i + 1) (j - 1) Beam) (i + 1) j (Split (n + 1))
          -- No need to update the position
          _ -> vs
        _ -> vs
      -- If the position is Nothing, Empty or Split, we ignore it
      _ -> vs

printGrid :: [[Cell]] -> IO ()
printGrid [] = putStrLn ""
printGrid (x : xs) = do
  mapM_ printCell x
  putStrLn ""
  printGrid xs
  where
    printCell :: Cell -> IO ()
    printCell Start = putStr "S"
    printCell Empty = putStr "."
    printCell Beam = putStr "|"
    printCell (Split {}) = putStr "^"

loop :: [[Cell]] -> [[Cell]]
loop xs =
  let loopHelper :: [[Cell]] -> [[Cell]] -> [[Cell]]
      loopHelper xs ys
        | xs == ys = ys
        | otherwise = loop ys
   in loopHelper xs $ iter xs $ getPos xs

getSplits :: [Cell] -> Int
getSplits [] = 0
getSplits (Split n : xs) = n + getSplits xs
getSplits (_ : xs) = getSplits xs

isSplit :: Cell -> Bool
isSplit (Split {}) = True
isSplit _ = False

isBeam :: Cell -> Bool
isBeam Beam = True
isBeam _ = False

hasSplit :: Cell -> Bool
hasSplit (Split 0) = False
hasSplit (Split {}) = True
hasSplit _ = False

data Tree
  = Tree
  { pos :: (Int, Int),
    kids :: [Tree]
  }

type Memo = [((Int, Int), Int)]

getKids :: [[Cell]] -> (Int, Int) -> [(Int, Int)]
getKids cs (i, j) = case get cs (i + 1) j of
  Just Beam -> [(i + 1, j)]
  Just (Split {}) -> [(i + 1, j - 1), (i + 1, j + 1)]
  _ -> []

dfs :: [[Cell]] -> (Int, Int) -> Memo -> (Int, Int) -> (Int, Memo)
dfs cs start memo dest
  | dest == start = (1, memo)
  | otherwise = case lookup start memo of
      Just n | n /= -1 -> (n, memo)
      _ -> foldl dfsHelper (0, memo) $ getKids cs start
  where
    dfsHelper :: (Int, Memo) -> (Int, Int) -> (Int, Memo)
    dfsHelper (n', memo') kid =
      let (n'', memo'') = dfs cs kid memo' dest
       in (n'' + n', (kid, n'') : memo'')

set :: (Eq a) => [(a, b)] -> a -> b -> [(a, b)]
set [] k v = [(k, v)]
set (x@(k', _) : xs) k v
  | k' == k = (k, v) : xs
  | otherwise = x : set xs k v

getEndPos :: [[Cell]] -> [(Int, Int)]
getEndPos cs =
  filterMaybe (\(x, y) -> isBeam <$> get cs x y) $
    [(length cs - 1, x) | x <- [0 .. length $ last cs]]

filterMaybe :: (a -> Maybe Bool) -> [a] -> [a]
filterMaybe _ [] = []
filterMaybe f (x : xs) = case f x of
  Just p | p -> x : filterMaybe f xs
  _ -> filterMaybe f xs

-- getPaths :: [[Cell]] -> Int
getPaths cs = scanl foo (0, []) ends
  where
    ends = getEndPos cs
    start = getStart cs
    sumMemo :: Memo -> Int
    sumMemo memo = sum $ mapMaybe (`lookup` memo) ends
    foo :: (Int, Memo) -> (Int, Int) -> (Int, Memo)
    foo (n, memo) dest =
      let (n', memo') = dfs cs start memo dest
       in (n + n', filter filterMemo memo')
    filterMemo :: ((Int, Int), Int) -> Bool
    filterMemo (_, 0) = False
    filterMemo (_, -1) = False
    filterMemo _ = True

foo :: [[Cell]] -> Int
foo cs = foldl foo 0 ends
  where
    ends = getEndPos cs
    start = getStart cs
    foo :: Int -> (Int, Int) -> Int
    foo n dest =
      let (n', memo') = dfs cs start [] dest
       in n + n'