{-# LANGUAGE BangPatterns #-}
import Data.IntMap.Strict (IntMap, findWithDefault, insert, empty)

type Tape = IntMap Int
type Step = (Tape, Int, State)
data State = State {
    zeroVal :: Int,
    oneVal  :: Int,
    zeroPos :: Int,
    onePos  :: Int,
    zeroState :: State,
    oneState  :: State
}

a, b, c, d, e, f :: State
a = State 1 0   1  (-1) b e
b = State 1 0 (-1)   1  c a
c = State 1 0 (-1)   1  d c
d = State 1 0 (-1) (-1) e f
e = State 1 1 (-1) (-1) a c
f = State 1 1 (-1)   1  e a

step :: Step -> Step
step (tape, position, (State zval oval zpos opos zstate ostate)) =
    case findWithDefault 0 position tape of
        0 -> (insert position zval tape, position + zpos, zstate)
        1 -> (insert position oval tape, position + opos, ostate)

stepN :: Int -> Step -> Step
stepN 0 s = s
stepN n s = let !nextStep = step s in stepN (n - 1) nextStep

main :: IO ()
main = do
    let (tape, _, _) = stepN 12386363 (empty, 0, a)
    print $ foldr (+) 0 tape