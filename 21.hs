import Data.List (transpose)
import Data.List.Split (splitOn, chunksOf)
import Data.HashMap (Map, insert, empty, (!))

type Rules = Map String String
squareRoot = floor . sqrt . fromIntegral

validTwos = [
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
valids str = map (map (str !!)) $ case length str of
    4 -> validTwos
    9 -> validThrees

chunk :: String -> [[String]]
chunk str =
    let size     = squareRoot $ length str
        multiple = if even size then 2 else 3
        makeRow  = map concat . transpose . map (chunksOf multiple) . chunksOf size
    in  map makeRow $ chunksOf (size * multiple) str

dechunk :: [[String]] -> String
dechunk rows =
    let multiple  = squareRoot . length . head . head $ rows
        unmakeRow = concat . concat . transpose . map (chunksOf multiple)
    in  concat $ map unmakeRow rows

enhance :: Rules -> String -> String
enhance rules = dechunk . map (map (rules !)) . chunk

parseLine :: String -> Rules -> Rules
parseLine line =
    let input : output : [] = splitOn " => " $ filter (/= '/') line
    in  flip (foldr (\key currRules -> insert key output currRules)) $ valids input

main :: IO ()
main = do
    rules <- foldr parseLine empty . lines <$> readFile "21.txt"
    let iterations = map (length . filter (=='#')) $ iterate (enhance rules) ".#...####"
    print $ iterations !! 5
    print $ iterations !! 18