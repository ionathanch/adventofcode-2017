import Data.List.Split (splitOn, chunksOf)
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

main :: IO ()
main = do
    input <- readFile "10.txt"
    let lengths    = map read $ splitOn "," input
        newLengths = map ord input ++ [17, 31, 73, 47, 23]
        (hashed,     _, _) =          hash lengths     ([0..255], 0, 0)
        (sparseHash, _, _) = iterate (hash newLengths) ([0..255], 0, 0) !! 64
        hexString     = concat . map (printf "%02x" . foldr xor 0) . chunksOf 16 $ sparseHash :: String
    print $ hashed !! 0 * hashed !! 1
    print $ hexString