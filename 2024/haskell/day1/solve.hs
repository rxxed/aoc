import Data.List
import System.IO.Unsafe

input :: [String]
input = unsafePerformIO $ do
    contents <- readFile "input"
    return $ words contents

cols :: ([Int], [Int])
cols = both (map snd) $ partition (even . fst) $ zip [0..] (map read input)
  where
    both f (t1, t2) = (f t1, f t2)

part1 = sum $ map (\(x,y) -> abs (x - y)) $ zip (sort $ fst cols) (sort $ snd cols)
part2 = sum [x * ((length . filter (== x) . snd) cols) | x <- fst cols]
