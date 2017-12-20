import Data.List.Split (splitOn)
import Data.Graph (reachable, scc)
import Data.Array (array)

parseLine :: String -> (Int, [Int])
parseLine str =
    let src : dests : [] = splitOn " <-> " str
    in  (read src, map read $ splitOn ", " dests)

main :: IO ()
main = do
    graph <- array (0, 1999) . map parseLine . lines <$> readFile "12.txt"
    print $ length $ reachable graph 0
    print $ length $ scc graph