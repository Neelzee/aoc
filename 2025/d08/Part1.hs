{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Data.List (sortBy)
import GHC.Float (int2Float)
import GHC.Internal.Data.List (minimumBy)

data JBox = JBox Int Int Int
  deriving (Show, Eq, Ord)

distanceJbox :: JBox -> JBox -> Float
distanceJbox (JBox p1 p2 p3) (JBox q1 q2 q3) = distance (p1, p2, p3) (q1, q2, q3)

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (p1, p2, p3) (q1, q2, q3) =
  let sqrSub :: Int -> Int -> Float
      sqrSub a b = int2Float (a - b) ** int2Float 2
   in sqrt (sqrSub p1 q1 + sqrSub p2 q2 + sqrSub p3 q3)

test :: String
test =
  "\
  \162,817,812\n\
  \57,618,57\n\
  \906,360,560\n\
  \592,479,940\n\
  \352,342,300\n\
  \466,668,158\n\
  \542,29,236\n\
  \431,825,988\n\
  \739,650,466\n\
  \52,470,668\n\
  \216,146,977\n\
  \819,987,18\n\
  \117,168,530\n\
  \805,96,715\n\
  \346,949,466\n\
  \970,615,88\n\
  \941,993,340\n\
  \862,61,35\n\
  \984,92,344\n\
  \425,690,689\
  \"

jboxParse :: String -> JBox
jboxParse s =
  let notComma :: Char -> Bool
      notComma = (/= ',')
   in JBox
        (read (takeWhile notComma s))
        (read (takeWhile notComma $ drop 1 $ dropWhile notComma s))
        (read (drop 1 $ dropWhile notComma $ drop 1 $ dropWhile notComma s))

parseData :: String -> [JBox]
parseData s = map jboxParse $ lines s

data Node
  = Single Int Int Int
  | Circuit [(Int, Int, Int)]
  deriving (Show, Eq)

addBox :: Node -> JBox -> Node
addBox (Single p1 p2 p3) (JBox q1 q2 q3) = Circuit [(p1, p2, p3), (q1, q2, q3)]
addBox (Circuit xs) (JBox q1 q2 q3) = Circuit $ (q1, q2, q3) : xs

connect :: [Node] -> [Node] -> [Node]
connect [] ns = ns
connect [x] ns = x : ns
connect (x : xs) ns = connect xs (add x closest : ns)
  where
    closest :: Node
    closest = minimumBy (\a b -> compare (nodeDistance a x) (nodeDistance b x)) xs
    add :: Node -> Node -> Node
    add (Single p1 p2 p3) (Single q1 q2 q3) = Circuit [(p1, p2, p3), (q1, q2, q3)]
    add (Circuit xs) (Single q1 q2 q3) = Circuit $ (q1, q2, q3) : xs
    add (Circuit xs) (Circuit ys) = Circuit $ xs ++ ys
    nodeDistance :: Node -> Node -> Float
    nodeDistance (Single p1 p2 p3) (Single q1 q2 q3) = distance (p1, p2, p3) (q1, q2, q3)
    nodeDistance (Circuit xs) (Single q1 q2 q3) =
      let cs = (q1, q2, q3)
       in distance cs $ minimumBy (\a b -> compare (distance a cs) (distance b cs)) xs

connectJBoxes :: [Node] -> JBox -> [Node]
connectJBoxes [] (JBox p1 p2 p3) = [Single p1 p2 p3]
connectJBoxes ns box =
  let closest :: Node
      closest = minimumBy (\a b -> compare (nodeDistance a box) (nodeDistance b box)) $ filter (notPos box) ns
      nodeDistance :: Node -> JBox -> Float
      nodeDistance (Single p1 p2 p3) (JBox q1 q2 q3) = distance (p1, p2, p3) (q1, q2, q3)
      nodeDistance (Circuit xs) (JBox q1 q2 q3) =
        let cs = (q1, q2, q3)
         in distance cs $ minimumBy (\a b -> compare (distance a cs) (distance b cs)) xs
      notPos :: JBox -> Node -> Bool
      notPos (JBox q1 q2 q3) (Single w1 w2 w3) = not (q1 == w1 && q2 == w2 && q3 == w2)
      notPos (JBox q1 q2 q3) (Circuit xs) = notElem (q1, q2, q3) xs
      ns' :: [Node]
      ns' = filter (/= closest) ns
   in addBox closest box : ns

circLength :: Node -> Int
circLength (Single {}) = 1
circLength (Circuit xs) = length xs

toSingle :: JBox -> Node
toSingle (JBox p1 p2 p3) = Single p1 p2 p3

minBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> (a, [a])
minBy _ [x] = (x, [])
minBy f xs = minByHelper f xs (head xs) (tail xs)
  where
    minByHelper :: (Ord a) => (a -> a -> Ordering) -> [a] -> a -> [a] -> (a, [a])
    minByHelper _ [] m xs = (m, xs)
    minByHelper f (x : xs) m ys = case f x m of
      LT -> minByHelper f xs x (m : ys)
      _ -> minByHelper f xs m (x : ys)

shortestDistance :: JBox -> JBox -> JBox -> Ordering
shortestDistance a b c = compare (distanceJbox a b) $ distanceJbox a c

foo :: [JBox] -> [[JBox]]
foo = bar []
  where
    bar :: [[JBox]] -> [JBox] -> [[JBox]]
    bar xs [] = xs
    bar [] [y] = error "This should not happen"
    bar [] (y : ys) =
      let (shortestYs, ys') = minBy (shortestDistance y) ys
       in bar [[y, shortestYs]] ys'
    bar xs [y] = addXs xs
      where
        shortestXs = fst $ minBy (shortestDistance y) $ concat xs
        addXs :: [[JBox]] -> [[JBox]]
        addXs [] = [[y, shortestXs]]
        addXs (z : zs)
          | shortestXs `elem` z = (y : z) : zs
          | otherwise = addXs zs
    bar xs (y : ys) =
      let (shortestYs, ys') = minBy (shortestDistance y) ys
          shortestXs = fst $ minBy (shortestDistance y) $ concat xs
          addXs :: [[JBox]] -> [[JBox]]
          addXs [] = [[y, shortestXs]]
          addXs (z : zs)
            | elem shortestXs z = (y : z) : zs
            | otherwise = addXs zs
       in case compare (distanceJbox y shortestYs) (distanceJbox y shortestXs) of
            GT -> bar (addXs xs) ys
            _ -> bar ([y, shortestYs] : xs) ys'

foobar :: Int -> [JBox] -> [JBox] -> [(JBox, JBox)]
foobar 0 xs _ = map (\x -> (x, x)) xs
foobar _ [] _ = []
foobar n (x : xs) zs =
  let (y, ys) = minBy (shortestDistance x) $ filter (/= x) zs
   in (x, y) : foobar (n - 1) (filter (/= y) xs) zs

pairsToNodes :: [(JBox, JBox)] -> [Node] -> [Node]
pairsToNodes [] xs = xs
pairsToNodes ((x, y) : xs) ys
  | x == y = pairsToNodes xs (toSingle x : ys)
  | isIn x y ys = pairsToNodes xs (addTo x y ys)
  | otherwise = pairsToNodes xs (Circuit [getPos x, getPos y] : ys)

getPos :: JBox -> (Int, Int, Int)
getPos (JBox p1 p2 p3) = (p1, p2, p3)

getPos' :: Node -> [(Int, Int, Int)]
getPos' (Single p1 p2 p3) = [(p1, p2, p3)]
getPos' (Circuit xs) = xs

isIn :: JBox -> JBox -> [Node] -> Bool
isIn _ _ [] = False
isIn a b (x : xs) = elem (getPos a) (getPos' x) || elem (getPos b) (getPos' x) || isIn a b xs

addTo :: JBox -> JBox -> [Node] -> [Node]
addTo x y [] = [Circuit [getPos x, getPos y]]
addTo x y (z : zs)
  | getPos x `elem` getPos' z = addBox z y : zs
  | getPos y `elem` getPos' z = addBox z x : zs
  | otherwise = z : addTo x y zs