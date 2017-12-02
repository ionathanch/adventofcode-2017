import Data.List

checksum :: [[Int]] -> Int
checksum = sum . map (\line -> maximum line - minimum line)

divsum :: [[Int]] -> Int
divsum = sum . map divline

divline :: [Int] -> Int
divline ns =
    let (n, m) = catTupleMaybes $ map (\x -> (x, find (greaterAndDivisible x) ns)) ns
    in  n `div` m
    where
        greaterAndDivisible :: Int -> Int -> Bool
        greaterAndDivisible n m = n > m && n `mod` m == 0

catTupleMaybes :: [(Int, Maybe Int)] -> (Int, Int)
catTupleMaybes [] = (0, 1)
catTupleMaybes ((t1, mt2):ts) = case mt2 of
    Just t2 -> (t1, t2)
    Nothing -> catTupleMaybes ts

main :: IO ()
main = do
    input <- readFile "2.txt"
    let grid = map (map read . words) $ lines input :: [[Int]]
    print $ checksum grid
    print $ divsum   grid