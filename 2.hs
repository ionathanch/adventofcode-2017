import Data.List
import Data.Maybe

checksum :: [[Int]] -> Int
checksum = sum . map (\line -> maximum line - minimum line)

divsum :: [[Int]] -> Int
divsum = sum . map divline

divline :: [Int] -> Int
divline ns = 
    let (n, m) = head $ catMaybes $ map (\x -> maybeTuplefy x $ find (greaterAndDivisible x) ns) ns
    in  n `div` m
    where   greaterAndDivisible :: Int -> Int -> Bool
            greaterAndDivisible n m = n > m && n `mod` m == 0
            maybeTuplefy :: Int -> Maybe Int -> Maybe (Int, Int)
            maybeTuplefy n mm = (,) <$> pure n <*> mm

main :: IO ()
main = do
    input <- readFile "2.txt"
    let grid = map (map read . words) $ lines input :: [[Int]]
    print $ checksum grid
    print $ divsum   grid