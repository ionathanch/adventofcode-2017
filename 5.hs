import Data.IntMap (IntMap, insert, fromList, findWithDefault)

type State = (Int, Int, IntMap Int)
type Update = Int -> Int

next :: Update -> State -> State
next f (steps, i, jumps) =
    let value = findWithDefault undefined i jumps
    in  (steps + 1, i + value, insert i (f value) jumps)

getExitSteps :: Int -> Update -> State -> Int
getExitSteps len f (steps, i, jumps) =
    if i >= len then steps else getExitSteps len f $! next f (steps, i, jumps)

main :: IO ()
main = do
    jumpsList <- fmap (map read . lines) $ readFile "5.txt"
    let jumpsMap  = fromList $ zip [0..] jumpsList
    print $ getExitSteps (length jumpsList) (+1)                                    (0, 0, jumpsMap)
    print $ getExitSteps (length jumpsList) (\v -> if v >= 3 then v - 1 else v + 1) (0, 0, jumpsMap)