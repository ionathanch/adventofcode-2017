{-# LANGUAGE TupleSections #-}

import Data.List (find)
import Data.Maybe (catMaybes)

checksum :: [[Int]] -> Int
checksum = sum . map (\line -> maximum line - minimum line)

divsum :: [[Int]] -> Int
divsum   = sum . map divline

divline :: [Int] -> Int
divline ns = 
    uncurry div . head . catMaybes . map (\x -> Just (x,) <*> find (greaterAndDivisible x) ns) $ ns
    where greaterAndDivisible n m = n > m && n `mod` m == 0

main :: IO ()
main = do
    input <- readFile "2.txt"
    let grid = map (map read . words) $ lines input :: [[Int]]
    print $ checksum grid
    print $ divsum   grid