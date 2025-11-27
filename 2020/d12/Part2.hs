import System.IO (IOMode (ReadMode), hGetContents, openFile)
import GHC.Float (int2Float, float2Int)

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
  let xs = parseData test
  let res = foldl evalInstr (Ship East (0, 0) (1, 10)) xs
  print res
  let (x, y) = manhattanDistance (position res) (waypoint res)
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
    position :: (Int, Int),
    waypoint :: (Int, Int)
  }
  deriving (Show, Eq)

evalInstr :: Ship -> Instr -> Ship
evalInstr s@(Ship _ _ (x, y)) (N i) = s { waypoint = (x + i, y) }
evalInstr s@(Ship _ _ (x, y)) (S i) = s { waypoint = (x - i, y) }
evalInstr s@(Ship _ _ (x, y)) (E i) = s { waypoint = (x, y + i) }
evalInstr s@(Ship _ _ (x, y)) (W i) = s { waypoint = (x, y - i) }
evalInstr s@(Ship _ spos wpos) (L i) = s { waypoint = turnLeft wpos i }
evalInstr s@(Ship _ spos wpos) (R i) = s { waypoint = turnRight wpos i }
evalInstr s@(Ship _ (sx, sy) (wx, wy)) (F i) = s { position = (sx + wx * i, sy + wy * i) }

turnLeft :: (Int, Int) -> Int -> (Int, Int)
turnLeft x 0 = x
turnLeft (x, y) n = turnLeft (x + 10, y - 1) (n - 90)

turnRight :: (Int, Int) -> Int -> (Int, Int)
turnRight x 0 = x
turnRight (x, y) n = turnRight (x - 10, y + 1) (n - 90)


manhattanDistance :: (Int, Int) -> (Int, Int) -> (Int, Int)
manhattanDistance (n, s) (e, w) = (abs (n - s), abs (e - w))