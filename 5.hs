import Data.IntMap (IntMap, insert, size, fromList, findWithDefault)

type Length = Int
type Index = Int
type Steps = Int
type State = (Steps, Index, IntMap Int)
type Update = Int -> Int

next :: Update -> State -> State
next f (steps, i, jumps) =
    let value = findWithDefault undefined i jumps
        nextI = i + value
        nextJumps = insert i (f value) jumps
    in  (steps + 1, nextI, nextJumps)

getExitSteps :: Length -> Update -> State -> Int
getExitSteps len f (steps, i, jumps) =
    if i >= len then steps else getExitSteps len f $! next f (steps, i, jumps)

main :: IO ()
main = do
    input <- readFile "5.txt"
    let jumpsList = map read $ lines input
        jumpsMap  = fromList $ zip [0..] jumpsList
        len = length jumpsList
    print $ getExitSteps len (+1)                                    (0, 0, jumpsMap)
    print $ getExitSteps len (\v -> if v >= 3 then v - 1 else v + 1) (0, 0, jumpsMap)