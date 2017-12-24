{-# LANGUAGE BangPatterns #-}
import Data.IntMap.Strict (IntMap, insert, fromList, findWithDefault)

type State = (Int, Int, IntMap Int)
type Update = Int -> Int

next :: Update -> State -> State
next f (steps, i, jumps) =
    let value = findWithDefault undefined i jumps
    in  (steps + 1, i + value, insert i (f value) jumps)

getExitSteps :: Int -> Update -> State -> Int
getExitSteps len f state@(!steps, i, _) =
    if i >= len then steps else let !nextState = next f state in getExitSteps len f nextState

main :: IO ()
main = do
    jumpsList <- map read . lines <$> readFile "05.txt"
    let jumpsMap  = fromList $ zip [0..] jumpsList
    print $ getExitSteps (length jumpsList) (+1)                                    (0, 0, jumpsMap)
    print $ getExitSteps (length jumpsList) (\v -> if v >= 3 then v - 1 else v + 1) (0, 0, jumpsMap)