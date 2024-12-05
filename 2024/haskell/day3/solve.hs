import System.IO.Unsafe
import Text.Regex.TDFA
import Data.List (elemIndex)
import Data.Maybe

input :: String
input = unsafePerformIO $ do
    contents <- readFile "input"
    return contents
--------------------------------------------------------------------------------
data Instruction = Do | Dont | Mul Int Int deriving Show

onlyMuls = getAllTextMatches $ input =~ "mul\\([0-9]+,[0-9]+\\)"
mulsDosAndDonts = getAllTextMatches $ input =~ "mul\\([0-9]+,[0-9]+\\)|(don't)\\(\\)|(do)\\(\\)"

parseInstruction :: String -> Instruction
parseInstruction s
  | take 4 s == "do()" = Do
  | take 7 s == "don't()" = Dont
  | take 4 s == "mul(" = Mul op1 op2
  where
    op1 = read $ slice (pal+1) (com-1) s
    op2 = read $ slice (com+1) (par-1) s
    pal = fromJust $ elemIndex '(' s -- will crash if '(' doesn't exist but whatever
    com = fromJust $ elemIndex ',' s -- same
    par = fromJust $ elemIndex ')' s -- same
    slice p q xs = drop p $ take (q+1) xs

interpret :: [Instruction] -> Bool -> Int
interpret [] _ = 0
interpret (Do:xs) _ = interpret xs True
interpret (Dont:xs) _ = interpret xs False
interpret (Mul m n:xs) considerMul = (if considerMul then m*n else 0) + interpret xs considerMul

part1 = interpret (map parseInstruction onlyMuls) True
part2 = interpret (map parseInstruction mulsDosAndDonts) True
