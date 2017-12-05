import Data.Sequence

type Index = Int
type Steps = Int
type State = (Steps, Index, Seq Int)
type Update = Int -> Int

next :: Update -> State -> State
next f (steps, i, jumps) =
    let value = index jumps i
        nextI = i + value
        nextJumps = update i (f value) jumps
    in  (steps + 1, nextI, nextJumps)

getExitSteps :: Update -> State -> Int
getExitSteps f (steps, i, jumps) =
    if i >= Data.Sequence.length jumps then steps else getExitSteps f $ next f (steps, i, jumps)

main :: IO ()
main = do
    input <- readFile "5.txt"
    let jumps = fromList . map read $ lines input :: Seq Int
    print $ getExitSteps (+1)                                    (0, 0, jumps)
    print $ getExitSteps (\v -> if v >= 3 then v - 1 else v + 1) (0, 0, jumps)