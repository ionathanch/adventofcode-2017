{-# LANGUAGE TupleSections #-}

import Data.List (find)
import Data.Maybe (catMaybes)

divline :: [Int] -> Int
divline ns = 
    uncurry div . head . catMaybes . map (\x -> Just (x,) <*> find (greaterAndDivisible x) ns) $ ns
    where greaterAndDivisible n m = n > m && n `mod` m == 0

main :: IO ()
main = do
    grid <- fmap (map (map read . words) . lines) $ readFile "2.txt"
    print $ sum . map (\line -> maximum line - minimum line) $ grid
    print $ sum . map divline $ grid