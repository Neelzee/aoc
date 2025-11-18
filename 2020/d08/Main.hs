module Main where
import System.IO (openFile, IOMode (ReadMode), hGetContents)


main :: IO ()
main = do
  handle <- openFile "./input" ReadMode
  contents <- hGetContents handle
  let instrs = map parseInstr $ lines contents
  --print $ eval instrs
  _ <- fixEval (VMFix (length instrs) (VM 0 0 []) instrs 0) instrs
  pure ()


parseInstr :: String -> Instruction
parseInstr ('n' : 'o' : 'p' : ' ' : '+' : xs ) = Nop (read xs :: Int)
parseInstr ('n' : 'o' : 'p' : ' ' : xs ) = Nop (read xs :: Int)
parseInstr ('a' : 'c' : 'c' : ' ' : '+' : xs ) = Acc (read xs :: Int)
parseInstr ('a' : 'c' : 'c' : ' ' : xs ) = Acc (read xs :: Int)
parseInstr ('j' : 'm' : 'p' : ' ' : '+' : xs ) = Jmp (read xs :: Int)
parseInstr ('j' : 'm' : 'p' : ' ' : xs ) = Jmp (read xs :: Int)

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show)

data VM =
  VM { acc :: Int
     , instrPtr :: Int
     , evaldInstr :: [Int]
     } deriving (Show)

eval :: [Instruction] -> Int
eval instr = length $ evaldInstr $ evalHelper (VM 0 0 [])
  where
    evalHelper :: VM -> VM
    evalHelper vm@(VM c ptr ev)
      | ptr `elem` ev = vm
      | otherwise = case instr !! ptr of
        (Acc i) -> evalHelper vm{ acc = c + i, instrPtr = ptr + 1, evaldInstr = ptr : ev }
        (Jmp i) -> evalHelper vm{ instrPtr = ptr + i, evaldInstr = ptr : ev }
        (Nop _) -> evalHelper vm{ instrPtr = ptr + 1, evaldInstr = ptr : ev }

data VMFix =
  VMFix { ptrBnd :: Int
        , vm :: VM
        , instr :: [Instruction]
        , instrSw :: Int
        } deriving (Show)


fixEval :: VMFix -> [Instruction] -> IO VM
fixEval vf@(VMFix bnd v is sw) xs = case safeEval xs v of
    Left _ -> case switchInstr sw is of
      (sw', is') -> fixEval vf{ instrSw = sw' } is'
    Right v' -> do
      print v'
      pure v

switchInstr :: Int -> [Instruction] -> (Int, [Instruction])
switchInstr lm is = case switchInstrHelper lm (drop lm is) of
  (lm', ls') -> (lm', take lm is ++ ls')

switchInstrHelper :: Int -> [Instruction] -> (Int, [Instruction])
switchInstrHelper i [] = error $ "outofbounds " ++ show i
switchInstrHelper c (x:xs) = case x of
      (Acc _) -> case switchInstrHelper (c + 1) xs of
          (c', ys) -> (c', x : ys)
      (Jmp i) -> (c + 1, Nop i : xs)
      (Nop i) -> (c + 1, Jmp i : xs)

safeEval :: [Instruction] -> VM -> Either VM VM
safeEval is v@(VM c ptr ev)
      | ptr `elem` ev = Left v
      | ptr >= length is = Right v
      | otherwise = case is !! ptr of
        (Acc i) -> safeEval is v{ acc = c + i, instrPtr = ptr + 1, evaldInstr = ptr : ev }
        (Jmp i) -> safeEval is v{ instrPtr = ptr + i, evaldInstr = ptr : ev }
        (Nop _) -> safeEval is v{ instrPtr = ptr + 1, evaldInstr = ptr : ev }