{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')
(%) = mod

ith :: Int -> Int -> Int
ith i steps =
    let (position, list) = foldl' (\(currPos, currList) n -> 
            let newPos  = (currPos + steps % n + 1) % n
            in  (newPos, take newPos currList ++ [n] ++ drop newPos currList))
            (0, [0]) [1..i]
    in  list !! (position + 1)

oneth :: Int -> Int -> Int
oneth i steps =
    snd $ foldl' (\(currPos, currOneth) n -> 
        let newPos = (currPos + steps % n + 1) % n
        in  (newPos, if newPos == 0 then n else currOneth)) 
        (0, 1) [1..i]

main :: IO ()
main = do
    print $ ith   2017     312
    print $ oneth 50000000 312 