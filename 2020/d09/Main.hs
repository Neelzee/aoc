import System.IO (openFile, IOMode (ReadMode), hGetContents)

xmasData :: [Int]
xmasData =
  [ 35
  , 20
  , 15
  , 25
  , 47
  , 40
  , 62
  , 55
  , 65
  , 95
  , 102
  , 117
  , 150
  , 182
  , 127
  , 219
  , 299
  , 277
  , 309
  , 576
  ]

main :: IO ()
main = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  let xmasData = mkXMASData contents
  let preamble = take 25 xmasData
  print $ validateXMAS 25 preamble $ drop 25 xmasData
  pure ()

testMain :: IO ()
testMain = do
  handle <- openFile "./test_input" ReadMode
  contents <- hGetContents handle
  let xmasData = mkXMASData contents
  let preamble = take 5 xmasData
  print $ validateXMAS 5 preamble $ drop 5 xmasData
  pure ()

mkXMASData :: String -> [Int]
mkXMASData xs = map (\x -> read x :: Int) $ lines xs

validateNum :: [Int] -> Int -> Bool
validateNum xs n = any validateNumHelper xs
  where
    validateNumHelper :: Int -> Bool
    validateNumHelper x = (n - x) `elem` xs

validateXMAS :: Int -> [Int] -> [Int] -> Int
validateXMAS _ _ [] = error "sadge"
validateXMAS ln preamble (x:xs)
  | validateNum (reverse $ take ln $ reverse preamble) x = validateXMAS ln (preamble ++ [x]) xs
  | otherwise = x

findContigousSum :: Int -> [Int] -> [Int]
findContigousSum trgt xs = helper 2
  where
    isSum :: [Int] -> Bool
    isSum xs = sum xs == trgt
    helper :: Int -> [Int]
    helper c = case first isSum (getSets c xs) of
      Just a -> a
      Nothing -> helper (c + 1)

getSets :: Int -> [a] -> [[a]]
getSets n xs = filter ((== n) . length) $ helper xs
  where
    helper :: [a] -> [[a]]
    helper [] = []
    helper ys = take n ys : helper (tail ys)

first :: (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first f (x:xs)
  | f x = Just x
  | otherwise = first f xs

main2 :: IO ()
main2 = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  let xmasData = mkXMASData contents
  let rs = findContigousSum 257342611 xmasData
  print rs
  print (minimum rs + maximum rs)
  pure ()


testMain2 :: IO ()
testMain2 = do
  handle <- openFile "./test_input" ReadMode
  contents <- hGetContents handle
  let xmasData = mkXMASData contents
  let rs = findContigousSum 127 xmasData
  print rs
  print (minimum rs + maximum rs)
  pure ()