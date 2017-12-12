import Data.List.Split (splitOn)
import Data.Graph (graphFromEdges, reachable, scc)

parseLine :: String -> (Int, Int, [Int])
parseLine str =
    let src : dests : [] = splitOn " <-> " str
    in  (read src, read src, map read $ splitOn ", " dests)

main :: IO ()
main = do
    (graph, _, _) <- fmap (graphFromEdges . map parseLine . lines) $ readFile "12.txt"
    print $ length $ reachable graph 0
    print $ length $ scc graph