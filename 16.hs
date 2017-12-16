{-# LANGUAGE BangPatterns #-}
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Char (ord, chr)
import Data.Bits ((.&.))
import Data.Function ((&))
import Data.Bimap (Bimap, fromList, toAscList, (!), (!>), insert)
import qualified Data.Bimap as B (map)

type Programs = Bimap Int Int
type Lookup   = Programs -> Int -> Int
type Insert   = Int -> Int -> Programs -> Programs

spin :: Int -> Programs -> Programs
spin s = B.map (\v -> (v + s) .&. 15)

swap :: Lookup -> Insert -> Int -> Int -> Programs -> Programs
swap (!!!) ins keyX keyY bimap =
    ins keyX (bimap !!! keyY) . ins keyY (bimap !!! keyX) $ bimap

parseMove :: String -> Programs -> Programs
parseMove str =
    let exchange = swap (!)  insert
        partner  = swap (!>) insertR
    in  case head str of
        's' ->  spin . read $ tail str
        'x' ->  let x    :    y : [] = splitOn "/" $ tail str
                in  exchange (read x)          (read y)
        'p' ->  let p : '/' : q : [] = tail str
                in  partner  (ord p - ord 'a') (ord q - ord 'a')
    where insertR b a = insert a b

dance :: [Programs -> Programs] -> Programs -> Programs
dance moves programs = foldl' (&) programs moves

danceN :: [Programs -> Programs] -> Programs -> Int -> Programs
danceN _     programs 0 = programs
danceN moves programs n = let !newPrograms = dance moves programs in danceN moves newPrograms (n-1)

getOrder :: Programs -> String
getOrder = map (chr . (+ ord 'a') . snd) . toAscList

main :: IO ()
main = do
    moves <- fmap (map parseMove . splitOn ",") $ readFile "16.txt"
    print $ getOrder $ dance  moves (fromList $ zip [0..15] [0..15])
    --print $ getOrder $ danceN moves (fromList $ zip [0..15] [0..15]) 1000000000