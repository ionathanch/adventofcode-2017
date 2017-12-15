import Data.Int (Int16)

gen :: Int -> Int -> Int -> [Int]
gen divisor factor seed = iterate (next divisor factor) seed
    where next d f i = (f * i) `mod` d

judge :: Int -> [Int] -> [Int] -> Int
judge n a b = length . filter (uncurry eq) . take n $ zip a b
    where eq i j = (fromIntegral i :: Int16) == (fromIntegral j :: Int16)

divisible :: Int -> Int -> Bool
divisible d = (== 0) . (`mod` d)

main :: IO ()
main = do
    let genA    = gen 2147483647 16807 634
        genB    = gen 2147483647 48271 301
        gen4A   = filter (divisible 4) genA
        gen8B   = filter (divisible 8) genB
    print $ judge 40000000 genA  genB 
    print $ judge 5000000  gen4A gen8B