import Data.List.Split (chunksOf)
import Data.Char (ord)
import Data.Bits (xor)
import Text.Printf (printf)
import Data.Graph (graphFromEdges, scc)
import Data.Sequence (Seq, index, fromList)

type Length = Int
type State = ([Int], Int, Int)
type Edge = (Int, Int, [Int])
(%)  = mod
(//) = div
(!)  = index

twist :: State -> Length -> State
twist (ring, position, skip) len =
    let rotated = slice position 256 $ cycle ring
        twisted = (reverse $ take len rotated) ++ drop len rotated
        newRing = slice (256 - position) 256 $ cycle twisted
    in  (newRing, (position + len + skip) % 256, skip + 1)
    where slice start amount = take amount . drop start
    
hash :: [Length] -> State -> State
hash lengths state = foldl twist state lengths

sparseHash :: [Int] -> String
sparseHash lengths =
    let (hashed, _, _) = iterate (hash lengths) ([0..255], 0, 0) !! 64
    in  concat . map (printf "%08b" . foldr xor 0) . chunksOf 16 $ hashed

getEdges :: Int -> (Seq Char, [Edge]) -> (Seq Char, [Edge])
getEdges ind (str, edges) = if str ! ind == '0' then (str, edges) else
    let row = ind // 128
        col = ind %  128
        neighbours = [(row + 1, col    ),
                      (row - 1, col    ),
                      (row,     col + 1),
                      (row,     col - 1)]
        validNeighbours = filter isOne . map toIndex . filter inBounds $ neighbours
    in  (str, (ind, ind, validNeighbours) : edges)
    where 
        inBounds (r, c) = r >= 0 && r <= 127 && c >= 0 && c <= 127
        toIndex  (r, c) = r * 128 + c
        isOne i = (str ! i) == '1'

main :: IO ()
main = do
    let hashes = concat $ map (sparseHash . (++ [17, 31, 73, 47, 23]) . map ord . ("ffayrhll-" ++) . show) [0..127]
        used = length . filter (== '1') $ hashes
        (_, edges)  = foldr getEdges (fromList hashes, []) [0..128 * 128 - 1]
        (graph, _, _) = graphFromEdges edges
    print $ used
    print $ length . scc $ graph