import Data.List (foldl', elemIndex)
import Data.List.Split (splitOn)
import Data.Function ((&))
import Data.Map.Strict (Map, (!), insert, fromList, toList, toAscList)

type Positions = Map Int  Int
type Swaps     = Map Char Char
type State     = (Int, Positions, Swaps)
(%) = mod

exchange :: Int -> Int -> State -> State
exchange x y (s, positions, swaps) =
    let spunX = (x + 16 - s) % 16
        spunY = (y + 16 - s) % 16
    in  (s, insert spunX (positions ! spunY) . insert spunY (positions ! spunX) $ positions, swaps)

partner  :: Char -> Char -> State -> State
partner p q (s, positions, swaps) =
    (s, positions, insert p (swaps ! q) . insert q (swaps ! p) $ swaps)

spin :: Int -> State -> State
spin s' (s, positions, swaps) = ((s + s') % 16, positions, swaps)

parseMove :: String -> State -> State
parseMove str =
    case head str of
        's' ->  spin . read $ tail str
        'x' ->  let x : y : [] = splitOn "/" $ tail str
                in  exchange (read x) (read y)
        'p' ->  let p : '/' : q : [] = tail str
                in  partner p q

dance :: [State -> State] -> State -> State
dance moves state = foldl' (&) state moves

applyDance :: State -> String -> String
applyDance (s, positions, swaps) str =
    let positionsList = snd . unzip . toAscList $ positions
        swapsReversed = fromList . (uncurry $ flip zip) . unzip . toList $ swaps
    in  map (swapsReversed !) . (drop (16 - s) <++> take (16 - s)) . map (str !!) $ positionsList
    where (f <++> g) p = f p ++ g p

main :: IO ()
main = do
    moves <- map parseMove . splitOn "," <$> readFile "16.txt"
    let ip = [0..15]; ap = ['a'..'p']
        state  = dance moves (0, fromList $ zip ip ip, fromList $ zip ap ap)
        dances = iterate (applyDance state) ap
        Just cycleSize = (+ 1) <$> (elemIndex ap $ tail dances)
    print $ dances !! 1
    print $ dances !! (1000000000 % cycleSize)