{-# LANGUAGE TemplateHaskell #-}
import Data.List
import Language.Haskell.TH
import System.IO.Unsafe

input :: [String]
input = unsafePerformIO $ do
    contents <- readFile "input"
    return $ words contents

fstCol :: [Int]
fstCol = [read x | (x,i) <- zip input [0..], even i]

sndCol :: [Int]
sndCol = [read x | (x,i) <- zip input [0..], odd i]

solve1 :: Int
solve1 = sum $ map (\(x,y) -> abs $ x - y) $ zip (sort fstCol) (sort sndCol)

solve2 :: Int
solve2 = sum [x * (length $ filter (== x) sndCol) | x <- fstCol]
