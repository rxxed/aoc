import Data.List
import System.IO.Unsafe

input :: [String]
input = unsafePerformIO $ do
    contents <- readFile "input"
    return $ lines contents

isSafe :: (Num a, Ord a) => [a] -> Bool
isSafe xs = allUnique xs && isSortedEitherWay xs && ((<4) . maximum . changes) (sort xs)
  where
    allUnique :: Eq a => [a] -> Bool
    allUnique xs = xs == nub xs
    isSortedEitherWay :: Ord a => [a] -> Bool
    isSortedEitherWay xs = xs == sort xs || xs == reverse (sort xs)
    changes :: Num a => [a] -> [a]
    changes xs = map abs $ zipWith (-) xs $ tail xs

isSafeWithDampening :: (Num a, Ord a) => [a] -> Bool
isSafeWithDampening xs = isSafe xs || any isSafe (sublists xs)
  where
    sublists :: [a] -> [[a]]
    sublists xs = zipWith (++) (inits xs) (tail $ tails xs)

solve1 :: Int
solve1 = length $ filter isSafe levels
  where
    levels = map (map read) $ map words input

solve2 :: Int
solve2 = length $ filter isSafeWithDampening levels
  where
    levels = map (map read) $ map words input
