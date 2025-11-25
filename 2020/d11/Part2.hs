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

test2 :: String
test2 =
  "\
  \.......#.\n\
  \...#.....\n\
  \.#.......\n\
  \.........\n\
  \..#L....#\n\
  \....#....\n\
  \.........\n\
  \#........\n\
  \...#.....\
  \"

test3 :: String
test3 =
  "\
  \.##.##.\n\
  \#.#.#.#\n\
  \##...##\n\
  \...L...\n\
  \##...##\n\
  \#.#.#.#\n\
  \.##.##.\
  \"

main :: IO ()
main = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  let xs = parseData contents
  let ys = loop xs (getNeighbourhood xs)
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
      helper p@(x, y) = case (xs !! x) !! y of
        Seat -> seatRule
        Floor -> Floor
        Occupied -> occupiedRule
        where
          seatRule :: Cell
          seatRule
            | all (getOccupado xs p) around = Occupied
            | otherwise = Seat
          occupiedRule :: Cell
          occupiedRule
            | 4 <= length (map (getOccupado xs p) around) = Seat
            | otherwise = Occupied
   in map (map helper) $ coords xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
  Just y -> y : mapMaybe f xs
  Nothing -> mapMaybe f xs

filterMaybe :: (a -> Maybe b) -> [a] -> [a]
filterMaybe _ [] = []
filterMaybe f (x : xs) = case f x of
  Just y -> x : filterMaybe f xs
  Nothing -> filterMaybe f xs

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

getOccupado :: [[Cell]] -> (Int, Int) -> (Int, Int) -> Bool
getOccupado xs src@(x, y) incr@(i, j) =
  let lst :: [Cell]
      lst = mapMaybe (get xs) (pos xs src incr)
   in elem Occupied lst || not (null lst)

getCell :: [[Cell]] -> (Int, Int) -> (Int, Int) -> (Cell, (Int, Int))
getCell xs src@(x, y) incr@(i, j) =
  let lst :: [(Cell, (Int, Int))]
      lst = mapMaybe (\x -> (,x) <$> get xs x) (pos xs src incr)
      eqCell :: (Cell, (Int, Int)) -> Bool
      eqCell (c, _) = Just c == get xs src
   in unsafeFirst eqCell lst

unsafeFirst :: (a -> Bool) -> [a] -> a
unsafeFirst _ [] = error "Could not find element on pred in list"
unsafeFirst f (x : xs)
  | f x = x
  | otherwise = unsafeFirst f xs

inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds xs x = case get xs x of
  Just _ -> True
  Nothing -> False

pos :: [[Cell]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
pos xs (x, y) incr@(i, j) = takeWhile (inBounds xs) $ scanl (\(acx, acy) (a, b) -> (acx + a, acy + b)) (x + i, y + j) $ repeat incr

loop :: [[Cell]] -> [((Int, Int), [(Int, Int)])] -> [[Cell]]
loop xs cs =
  let helper :: [[Cell]] -> [[Cell]] -> [[Cell]]
      helper ys zs
        | ys == zs = ys
        | otherwise = helper zs (iter2 zs cs)
   in helper xs (iter2 xs cs)

getNeighbourhood :: [[Cell]] -> [((Int, Int), [(Int, Int)])]
getNeighbourhood xs = map (\x -> (x, mapMaybe (getSights xs x) around)) $ concat (coords xs)

getSights :: [[Cell]] -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
getSights xs src@(x, y) incr@(i, j) =
  let lst :: [(Int, Int)]
      lst = filterMaybe (get xs) (pos xs src incr)
      cell :: Cell
      cell = (xs !! x) !! y
      find :: [(Int, Int)] -> Maybe (Int, Int)
      find [] = Nothing
      find (y : ys) = case get xs y of
        Just z | z /= Floor -> Just y
        _ -> find ys
   in find lst

iter2 :: [[Cell]] -> [((Int, Int), [(Int, Int)])] -> [[Cell]]
iter2 cs xs =
  let updateCells :: [[Cell]] -> ((Int, Int), [(Int, Int)]) -> [[Cell]]
      updateCells cs' (pos, ys) = case get cs pos of
        Just Seat | Occupied `notElem` mapMaybe (get cs) ys -> put pos Occupied cs'
        Just Occupied | 5 <= length (filter (== Occupied) (mapMaybe (get cs) ys)) -> put pos Seat cs'
        Just x -> cs'
        _ -> error "this should not occur"
   in foldl updateCells cs xs

put :: (Int, Int) -> a -> [[a]] -> [[a]]
put (x, y) a xs =
  let row = xs !! x
      row' = take y row ++ (a : drop (y + 1) row)
   in take x xs ++ (row' : drop (x + 1) xs)