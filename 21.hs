import Data.List (transpose)
import Data.List.Split (splitOn, chunksOf)
import Data.Map.Strict (Map, insert, empty, (!))

type Rules = Map String String
squareRoot = floor . sqrt . fromIntegral

validTwos   = [
        [0..3],
        [0, 2, 1, 3],
        [1, 0, 3, 2],
        [1, 3, 0, 2],
        [2, 0, 3, 1],
        [2, 3, 0, 1],
        [3, 1, 2, 0],
        [3,2..0]
    ]

validThrees = [
        [0..8],
        [0, 3, 6, 1, 4, 7, 2, 5, 8],
        [2, 1, 0, 5, 4, 3, 8, 7, 6],
        [2, 5, 8, 1, 4, 7, 0, 3, 6],
        [6, 3, 0, 7, 4, 1, 8, 5, 2],
        [6, 7, 8, 3, 4, 5, 0, 1, 2],
        [8, 5, 2, 7, 4, 1, 6, 3, 0],
        [8,7..0]
    ]

valids :: String -> [String]
valids str = case length str of
    4 -> map (map (str !!)) validTwos
    9 -> map (map (str !!)) validThrees

row :: Int -> Int -> String -> [String]
row multiple size str =
    map concat . transpose . map (chunksOf multiple) . chunksOf size $ str

derow :: Int -> [String] -> String
derow multiple sbsq =
    concat . concat . transpose . map (chunksOf multiple) $ sbsq

chunk :: String -> [[String]]
chunk str =
    let size = squareRoot $ length str
        multiple = if even size then 2 else 3
        rows = chunksOf (size * multiple) str
    in  map (row multiple size) rows

dechunk :: [[String]] -> String
dechunk rows =
    let multiple = squareRoot . length . head . head $ rows
    in  concat $ map (derow multiple) rows

enhance :: Rules -> String -> String
enhance rules grid =
    dechunk . map (map (rules !)) . chunk $ grid

addRules :: [String] -> String -> Rules -> Rules
addRules keys value rules = foldr (\key currRules -> insert key value currRules) rules keys

countOn :: String -> Int
countOn grids = length . filter (== '#') $ grids

parseLine :: String -> Rules -> Rules
parseLine line rules =
    let inputStr : outputStr : [] = splitOn " => " line
        input  = splitOn "/" inputStr
        output = splitOn "/" outputStr
    in  addRules (valids $ concat input) (concat output) rules

main :: IO ()
main = do
    rules <- foldr parseLine empty . lines <$> readFile "21.txt"
    let iterations = map countOn $ iterate (enhance rules) ".#...####"
    print $ iterations !! 5
    print $ iterations !! 18