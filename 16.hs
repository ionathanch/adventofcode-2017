{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Bits ((.&.))
import Data.Function ((&))
import Data.Bimap (Bimap, fromList, toAscList, (!), (!>), insert)

type Programs = Bimap Int Char
type State    = (Int, Programs)

exchange :: Int -> Int -> State -> State
exchange x y (s, programs) =
    let spunX = (x + 16 - s) .&. 15
        spunY = (y + 16 - s) .&. 15
    in  (s, insert spunX (programs ! spunY) . insert spunY (programs ! spunX) $ programs)

partner  :: Char -> Char -> State -> State
partner  p q (s, programs) =
    (s, insert (programs !> q) p . insert (programs !> p) q $ programs)

addSpin :: Int -> State -> State
addSpin s' (s, p) = ((s + s') .&. 15, p)

spin :: Int -> String -> String
spin s str = drop (16 - s) str ++ take (16 - s) str

parseMove :: String -> State -> State
parseMove str =
    case head str of
        's' ->  addSpin . read $ tail str
        'x' ->  let x : y : [] = splitOn "/" $ tail str
                in  exchange (read x) (read y)
        'p' ->  let p : '/' : q : [] = tail str
                in  partner p q

dance :: [State -> State] -> State -> State
dance moves state = foldl' (&) state moves

danceN :: [State -> State] -> State -> Int -> State
danceN _     state 0 = state
danceN moves state n = let !newState = dance moves state in danceN moves newState (n-1)

getOrder :: State -> String
getOrder (s, programs) = spin s . snd . unzip . toAscList $ programs

main :: IO ()
main = do
    moves <- fmap (map parseMove . splitOn ",") $ readFile "16.txt"
    print $ getOrder $ danceN moves (0, fromList $ zip [0..15] ['a'..'p']) 1