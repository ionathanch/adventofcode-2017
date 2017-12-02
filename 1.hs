import Text.Read
import Data.Maybe

circularSum :: [Int] -> Int
circularSum ns = snd $ foldr (\n (prev, sum) -> (n, if n == prev then n + sum else sum)) (last ns, 0) ns

halfwaySum :: [Int] -> Int
halfwaySum ns =
    let ms = drop (length ns `div` 2) ns ++ take (length ns `div` 2) ns
        nms = zip ns ms
    in  foldr (\(n, m) sum -> if n == m then n + sum else sum) 0 nms

main :: IO ()
main = do 
    input <- readFile "1.txt"
    let nums = catMaybes $ map (readMaybe . (:[])) input :: [Int]
    print $ circularSum nums
    print $ halfwaySum nums