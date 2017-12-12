import Data.Vector.Unboxed (Vector, fromList, (!), (//))
import qualified Data.Vector.Unboxed as V (length)

type Length = Int
type Index = Int
type Steps = Int
type State = (Steps, Index, Vector Int)
type Update = Int -> Int

next :: Update -> State -> State
next f (steps, i, jumps) =
    let value = jumps ! i
        nextI = i + value
        nextJumps = jumps // [(i, f value)]
    in  (steps + 1, nextI, nextJumps)

getExitSteps :: Update -> State -> Int
getExitSteps f (steps, i, jumps) =
    if i >= V.length jumps then steps else getExitSteps f $! next f (steps, i, jumps)

main :: IO ()
main = do
    input <- readFile "5.txt"
    let jumpsList = fromList $ map read $ lines input
    print $ getExitSteps (+1)                                    (0, 0, jumpsList)
    print $ getExitSteps (\v -> if v >= 3 then v - 1 else v + 1) (0, 0, jumpsList)