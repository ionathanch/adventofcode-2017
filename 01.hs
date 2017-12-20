circularSum :: [Int] -> Int
circularSum ns = snd $ foldr (\n (prev, sum) -> (n, n * fromEnum (n == prev) + sum)) (last ns, 0) ns

halfwaySum :: [Int] -> Int
halfwaySum ns =
    let ms = drop (length ns `div` 2) ns ++ take (length ns `div` 2) ns
    in  foldr (\(n, m) sum -> n * fromEnum (n == m) + sum) 0 $ zip ns ms

main :: IO ()
main = do 
    nums <- map (read . pure) <$> readFile "01.txt"
    print $ circularSum nums
    print $ halfwaySum nums