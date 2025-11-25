import Data.Bifunctor (Bifunctor (bimap))
import Data.Bifunctor qualified
import System.IO (IOMode (ReadMode), hGetContents, openFile)

test :: String
test =
  "\
  \L.LL.LL.LL\n\
  \LLLLLLL.LL\n\
  \L.L.L..L..\n\
  \LLLL.LL.LL\n\
  \L.LL.LL.LL\n\
  \L.LLLLL.LL\n\
  \..L.L.....\n\
  \LLLLLLLLLL\n\
  \L.LLLLLL.L\n\
  \L.LLLLL.LL\
  \"

main :: IO ()
main = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  let xs = parseData contents
  let ys = loop xs
  print $ sum $ map (length . filter (== Occupied)) ys
  pure ()

data Cell
  = Seat
  | Floor
  | Occupied
  deriving (Show, Eq)

parseCell :: Char -> Cell
parseCell 'L' = Seat
parseCell '.' = Floor
parseCell '#' = Occupied
parseCell c = error $ "Invalid char: " ++ show c

parseData :: String -> [[Cell]]
parseData xs = map (map parseCell) $ lines xs

iter :: [[Cell]] -> [[Cell]]
iter xs =
  let helper :: (Int, Int) -> Cell
      helper (x, y) = case (xs !! x) !! y of
        Seat -> seatRule
        Floor -> Floor
        Occupied -> occupiedRule
        where
          seatRule :: Cell
          seatRule
            | notElem Occupied (concatMap (filter (== Occupied) . getSight xs (x, y)) around) = Occupied
            | otherwise = Seat
          occupiedRule :: Cell
          occupiedRule
            | 4 <= length (concatMap (filter (== Occupied) . getSight xs (x, y)) around) = Seat
            | otherwise = Occupied
   in map (map helper) $ coords xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs

get :: [[a]] -> (Int, Int) -> Maybe a
get xs (x, y)
  | x < 0 || y < 0 = Nothing
  | x >= length xs = Nothing
  | y >= length (xs !! x) = Nothing
  | otherwise = Just $ (xs !! x) !! y

coords :: [[a]] -> [[(Int, Int)]]
coords xs = [[(x, y) | y <- [0 .. (length (head xs) - 1)]] | x <- [0 .. (length xs - 1)]]

around :: [(Int, Int)]
around =
  [ (-1, -1),
    (0, -1),
    (1, -1),
    (-1, 0),
    (1, 0),
    (-1, 1),
    (0, 1),
    (1, 1)
  ]

getSight :: [[Cell]] -> (Int, Int) -> (Int, Int) -> [Cell]
getSight xs src@(x, y) incr@(i, j) = mapMaybe (get xs) $ scanr (\(acx, acy) (x, y) -> (acx + x, acy + y)) (x + i, y + j) (replicate 99 incr)

loop :: [[Cell]] -> [[Cell]]
loop xs =
  let helper :: Int -> [[Cell]] -> [[Cell]] -> [[Cell]]
      helper c ys zs
        | ys == zs = ys
        | otherwise = helper (c + 1) zs (iter zs)
   in helper 1 xs (iter xs)