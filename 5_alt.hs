import Data.Vector.Unboxed (Vector, fromList, (!), (//))
import qualified Data.Vector.Unboxed as V (length)

type State = (Int, Int, Vector Int)
type Update = Int -> Int

next :: Update -> State -> State
next f (steps, i, jumps) =
    let value = jumps ! i
    in  (steps + 1, i + value, jumps // [(i, f value)])

getExitSteps :: Update -> State -> Int
getExitSteps f (steps, i, jumps) =
    if i >= V.length jumps then steps else getExitSteps f $! next f (steps, i, jumps)

main :: IO ()
main = do
    jumpsList <- fmap (fromList . map read . lines) $ readFile "5.txt"
    print $ getExitSteps (+1)                                    (0, 0, jumpsList)
    print $ getExitSteps (\v -> if v >= 3 then v - 1 else v + 1) (0, 0, jumpsList)