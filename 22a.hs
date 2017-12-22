import Data.HashMap.Strict (HashMap, lookupDefault, insert, empty)

data Direction = North | East | South | West deriving Enum
type Grid = HashMap (Int, Int) Char
type State = (Grid, (Int, Int), Direction, Int)
(%) = mod

changeDirection :: Char -> Direction -> Direction
changeDirection c = case c of
    '#' -> toEnum . (%4) . (+1) . fromEnum
    '.' -> toEnum . (%4) . (+3) . fromEnum

incrementPosition :: Direction -> (Int, Int) -> (Int, Int)
incrementPosition dir (x, y) = case dir of
    North -> (x, y - 1)
    East  -> (x + 1, y)
    South -> (x, y + 1)
    West  -> (x - 1, y)

nextState :: State -> State
nextState (grid, pos, dir, count) =
    let currNode = lookupDefault '.' pos grid
        newDir   = changeDirection currNode dir
        newGrid  = insert pos (if currNode == '.' then '#' else '.') grid
        newPos   = incrementPosition newDir pos
        newCount = count + if currNode == '.' then 1 else 0
    in  (newGrid, newPos, newDir, newCount)

parseRow :: (Int, [(Int, Char)]) -> Grid -> Grid
parseRow (y, xs) grid = foldr (\(x, c) currGrid -> insert (x, y) c currGrid) grid xs

main :: IO ()
main = do
    input <- readFile "22.txt"
    let grid = foldr parseRow empty $ zip [-12..12] . map (zip [-12..12]) . lines $ input
        (_, _, _, count) = iterate nextState (grid, (0, 0), North, 0) !! 10000
    print $ count