{-# LANGUAGE BangPatterns #-}
import Data.Function (on)
import Data.Bits ((.&.))
        
count :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Int -> Int
count divisor (factorA, factorB) (seedA, seedB) (divA, divB) acc times =
    if times == 0 then acc else
    let !nextA = nextMaskBy factorA divA seedA
        !nextB = nextMaskBy factorB divB seedB
        !eq = fromEnum $ ((==) `on` (.&. 0xffff)) nextA nextB
    in count divisor (factorA, factorB) (nextA, nextB) (divA, divB) (acc + eq) (times - 1)
    where 
        nextMaskBy f d s = let t = (f * s) `mod` divisor in if (t .&. d) == 0 then t else nextMaskBy f d t

main :: IO ()
main = do
    print $ count 2147483647 (16807, 48271) (634, 301) (0, 0) 0 40000000
    print $ count 2147483647 (16807, 48271) (634, 301) (3, 7) 0 5000000