{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

import Control.Monad.State (MonadState (..), State (..))
import Data.List (sort)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

test :: String
test =
  "\
  \16\n\
  \10\n\
  \15\n\
  \5\n\
  \1\n\
  \11\n\
  \7\n\
  \19\n\
  \6\n\
  \12\n\
  \4\
  \"

main :: IO ()
main = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  let xs = parseData contents
  let max = last xs
  let min = head xs
  print $ fst $ dfsState xs max min $ map (,-1) xs
  pure ()

parseData :: String -> [Int]
parseData xs =
  let ys :: [Int]
      ys = sort $ map read $ lines xs
   in 0 : ys ++ [last ys + 3]

kids :: [Int] -> Int -> [Int]
kids xs val = filter (\x -> (x - val) <= 3 && (x - val) > 0 && x /= val) xs

countPaths :: [Int] -> Int -> Int -> Int
countPaths dag target cur
  | target == cur = 1
  | otherwise = sum $ map (countPaths dag target) currentKids
  where
    currentKids :: [Int]
    currentKids = revKids dag cur

revKids :: [Int] -> Int -> [Int]
revKids xs val = filter (\x -> (val - x) <= 3 && (val - x) > 0 && x /= val) xs

adjList :: [Int] -> [(Int, [Int])]
adjList [] = []
adjList (x : xs) = (x, kids xs x) : adjList xs

dfsState :: [Int] -> Int -> Int -> [(Int, Int)] -> (Int, [(Int, Int)])
dfsState verticies dest src memo
  | src == dest = (1, memo)
  | otherwise = case lookup src memo of
      Just r | r /= -1 -> (r, memo)
      _ ->
        let foldlHelper :: (Int, [(Int, Int)]) -> Int -> (Int, [(Int, Int)])
            foldlHelper (acc, memo') v =
              let (p, memo'') = dfsState verticies dest v memo'
               in (acc + p, memo'')
            (count, memo') = foldl foldlHelper (0, memo) (kids verticies src)
         in (count, unsafePut src count memo')

unsafeLookup :: (Eq a) => a -> [(a, b)] -> b
unsafeLookup _ [] = error "Key does not exist in map"
unsafeLookup k ((x, v) : xs)
  | k == x = v
  | otherwise = unsafeLookup k xs

unsafePut :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
unsafePut _ _ [] = error "Key does not exist in map"
unsafePut k v (x@(k', _) : xs)
  | k == k' = (k, v) : xs
  | otherwise = x : unsafePut k v xs