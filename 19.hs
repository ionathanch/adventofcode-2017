import Data.List (elemIndex)
import Data.Matrix (Matrix, fromLists, (!))

type Grid = Matrix Char
data Direction = North | South | West | East | Stop deriving Eq
data State = State (Int, Int) Direction [Char] Int

nextState :: String -> Grid -> State -> State
nextState letters grid (State (r, c) dir seen count) =
    let (nextR, nextC) = case dir of
            North -> (r - 1, c)
            South -> (r + 1, c)
            West  -> (r, c - 1)
            East  -> (r, c + 1)
        nextChar = grid ! (nextR, nextC)
        nextDir  = case nextChar of
            '+' ->  if dir `elem` [North, South]
                    then if grid ! (nextR, nextC - 1) == '-' then West  else East
                    else if grid ! (nextR - 1, nextC) == '|' then North else South
            ' ' ->  Stop
            _   ->  dir
        nextSeen = if nextChar `elem` letters then nextChar : seen else seen
    in  State (nextR, nextC) nextDir nextSeen (count + 1)

traverseGrid :: String -> Grid -> State -> (String, Int)
traverseGrid _ _ (State _ Stop seen count) = (reverse seen, count)
traverseGrid letters grid state = traverseGrid letters grid $ nextState letters grid state

main :: IO ()
main = do
    input <- readFile "19.txt"
    let rows = lines input
        Just start = (+1) <$> (elemIndex '|' $ head rows)
        letters = filter (not . (`elem` " +|-\n")) input
    print $ traverseGrid letters (fromLists rows) (State (1, start) South [] 0)