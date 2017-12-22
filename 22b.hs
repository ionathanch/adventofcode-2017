{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
import Data.HashMap.Strict (HashMap, lookupDefault, insert, empty)

data Direction = North    | East     | South   | West  deriving (Bounded, Enum)
data Node      = Weakened | Infected | Flagged | Clean deriving (Bounded, Enum, Eq)
type Grid  = HashMap (Int, Int) Node
type State = (Grid, (Int, Int), Direction, Int)
(%) = mod; infixl 5 %

succn :: forall a. (Bounded a, Enum a) => Int -> a -> a
succn n = toEnum . (% 1 + fromEnum (maxBound :: a)) . (+ n) . fromEnum

incrementPosition :: Direction -> (Int, Int) -> (Int, Int)
incrementPosition dir (x, y) = case dir of
    North -> (x, y - 1)
    East  -> (x + 1, y)
    South -> (x, y + 1)
    West  -> (x - 1, y)

nextState :: State -> State
nextState (grid, pos, dir, count) =
    let currNode = lookupDefault Clean pos grid
        newDir   = succn (fromEnum currNode) dir
        newGrid  = insert pos (succn 1 currNode) grid
        newPos   = incrementPosition newDir pos
        !newCount = count + fromEnum (currNode == Weakened)
    in  (newGrid, newPos, newDir, newCount)

stricterate :: Int -> State -> Int
stricterate 0 (_, _, _, count) = count
stricterate n state = let !next = nextState state in stricterate (n-1) next

parseRow :: (Int, [(Int, Char)]) -> Grid -> Grid
parseRow (y, xs) grid = foldr (\(x, c) currGrid -> insert (x, y) (charToEnum c) currGrid) grid xs
    where charToEnum c = case c of
            '.' -> Clean
            '#' -> Infected

main :: IO ()
main = do
    grid <- foldr parseRow empty . zip [-12..12] . map (zip [-12..12]) . lines <$> readFile "22.txt"
    print $ stricterate 10000000 (grid, (0, 0), North, 0)