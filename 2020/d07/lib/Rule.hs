module Rule where

data Rule = Rule
  { colour :: String
  , bags :: [String]
  }
  deriving (Show)

instance Eq Rule where
  (==) (Rule l _) (Rule r _) = l == r

getColour :: String -> String
getColour xs = unwords $ takeWhile (/= "bags") $ words xs

unColour :: String -> String
unColour xs = unwords $ tail $ dropWhile (/= "contain") $ words xs

splitChildren :: String -> [String]
splitChildren [] = []
splitChildren xs
  | ',' `elem` xs = takeWhile (/= ',') xs : splitChildren (drop 2 $ dropWhile (/= ',') xs)
  | otherwise = [takeWhile (/= '.') xs]

parseChild :: String -> [String]
parseChild xs = replicate (getChildCount xs) $ getChildColour xs

getChildCount :: String -> Int
getChildCount xs = let
    el = head $ words xs
    count = read el
  in
    if el == "no" then
      0
    else
      count

getChildColour :: String -> String
getChildColour xs = unwords $ filter (/= "bag") $ filter (/= "bags") $ words $ (trim . tail) xs

trim :: String -> String
trim (' ':xs) = trim xs
trim xs = xs

getChildren :: String -> [String]
getChildren xs = concatMap parseChild $ splitChildren $ unColour xs
