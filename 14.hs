import Data.List.Split (chunksOf)
import Data.Char (ord)
import Data.Bits (xor)
import Text.Printf (printf)

type Length = Int
type State = ([Int], Int, Int)
(%) = mod

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

main :: IO ()
main = do
    let hashes = map (sparseHash . (++ [17, 31, 73, 47, 23]) . map ord . ("ffayrhll-" ++) . show) [0..127]
        used   = length . filter (== '1') . concat $ hashes
    print $ used
    mapM_ print hashes