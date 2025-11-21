import Data.List (sort)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

main :: IO ()
main = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  print $ findJoltDifference $ parseData contents

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

parseData :: String -> [Int]
parseData xs = map read $ lines xs

data JoltDiff
  = JoltDiff
  { one :: Int,
    three :: Int
  }
  deriving (Show)

findJoltDifference :: [Int] -> Int
findJoltDifference xs =
  let helper :: [Int] -> JoltDiff -> JoltDiff
      helper [x, y] jd@(JoltDiff o t)
        | y - x == 1 = jd {one = o + 1}
        | y - x == 3 = jd {three = t + 1}
        | otherwise = jd
      helper (x : y : ys) jd@(JoltDiff o t)
        | y - x == 1 = helper (y : ys) jd {one = o + 1}
        | y - x == 3 = helper (y : ys) jd {three = t + 1}
        | otherwise = helper (y : ys) jd
      result :: JoltDiff
      result = helper (sort xs) $ JoltDiff {one = 1, three = 1}
   in one result * three result