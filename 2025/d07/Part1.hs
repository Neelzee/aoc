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
  print $ length $ concatMap (filter hasSplit) (loop xs)
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
get xs x y
  | x < 0 || y < 0 = Nothing
  | x >= length xs = Nothing
  | y >= length (xs !! x) = Nothing
  | otherwise = Just $ (xs !! x) !! y

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

hasSplit :: Cell -> Bool
hasSplit (Split 0) = False
hasSplit (Split {}) = True
hasSplit _ = False