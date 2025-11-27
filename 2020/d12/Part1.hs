import System.IO (IOMode (ReadMode), hGetContents, openFile)

test :: String
test =
  "\
  \F10\n\
  \N3\n\
  \F7\n\
  \R90\n\
  \F11\
  \"

main :: IO ()
main = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  let xs = parseData contents
  let res = foldl evalInstr (Ship East 0 0 0 0) xs
  print res
  let (x, y) = manhattanDistance (north res, south res, east res, west res)
  print $ x + y
  pure ()

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

data Instr
  = N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int
  deriving (Show, Eq)

parseInstr :: String -> Instr
parseInstr ('N' : xs) = N (read xs)
parseInstr ('S' : xs) = S (read xs)
parseInstr ('E' : xs) = E (read xs)
parseInstr ('W' : xs) = W (read xs)
parseInstr ('L' : xs) = L (read xs)
parseInstr ('R' : xs) = R (read xs)
parseInstr ('F' : xs) = F (read xs)
parseInstr c = error $ "invalid parse: " ++ c

parseData :: String -> [Instr]
parseData xs = map parseInstr $ lines xs

data Ship
  = Ship
  { heading :: Direction,
    north :: Int,
    south :: Int,
    east :: Int,
    west :: Int
  }
  deriving (Show, Eq)

mkInstr :: Direction -> Int -> Instr
mkInstr North i = N i
mkInstr South i = S i
mkInstr East i = E i
mkInstr West i = W i

evalInstr :: Ship -> Instr -> Ship
evalInstr s (N i) = s {north = north s + i}
evalInstr s (S i) = s {south = south s + i}
evalInstr s (E i) = s {east = east s + i}
evalInstr s (W i) = s {west = west s + i}
evalInstr s (L i) = s {heading = turnLeft (heading s) i}
evalInstr s (R i) = s {heading = turnRight (heading s) i}
evalInstr s (F i) = evalInstr s (mkInstr (heading s) i)

turnLeft :: Direction -> Int -> Direction
turnLeft x 0 = x
turnLeft North x
  | x == 90 = West
  | otherwise = turnLeft West (x - 90)
turnLeft South x
  | x == 90 = East
  | otherwise = turnLeft East (x - 90)
turnLeft West x
  | x == 90 = South
  | otherwise = turnLeft South (x - 90)
turnLeft East x
  | x == 90 = North
  | otherwise = turnLeft North (x - 90)

turnRight :: Direction -> Int -> Direction
turnRight x 0 = x
turnRight North x
  | x == 90 = East
  | otherwise = turnRight East (x - 90)
turnRight South x
  | x == 90 = West
  | otherwise = turnRight West (x - 90)
turnRight West x
  | x == 90 = North
  | otherwise = turnRight North (x - 90)
turnRight East x
  | x == 90 = South
  | otherwise = turnRight South (x - 90)

manhattanDistance :: (Int, Int, Int, Int) -> (Int, Int)
manhattanDistance (n, s, e, w) = (abs (n - s), abs (e - w))