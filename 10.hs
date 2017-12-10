import Data.List.Split (splitOn)
import Data.Sequence (Seq, update, fromList)
import qualified Data.Sequence as S (lookup)
import Data.Char (ord)
import Data.Bits (xor)
import Data.Foldable (toList)
import Numeric (showHex)

type Ring = Seq Int
type Position = Int
type Skip = Int
type Length = Int
type State = (Ring, Position, Skip)

(%) = mod

unsafeLookup :: Int -> Seq Int -> Int
unsafeLookup index seq =
    case S.lookup index seq of
        Just i -> i

twist :: State -> Length -> State
twist (ring, position, skip) len =
    let end       = position + len - 1
        positions = map (% 256) [position..end]
        posRev    = zip positions $ reverse positions
        newRing   = foldl (\currRing (pos, rev) -> update pos (unsafeLookup rev ring) currRing) ring posRev
    in  (newRing, (end + skip + 1) % 256, skip + 1)

hash :: [Length] -> State -> State
hash lengths state = foldl twist state lengths

condense :: [Int] -> [Int]
condense [] = []
condense ns =
    let block = foldr xor 0 $ take 16 ns
    in  block : (condense   $ drop 16 ns)

showHexPrepended :: Int -> (String -> String)
showHexPrepended n
    | n < 15    = showHex 0 . showHex n
    | otherwise = showHex n

main :: IO ()
main = do
    input <- readFile "10.txt"
    let lengths    = map read $ splitOn "," input
        newLengths = map ord input ++ [17, 31, 73, 47, 23]
        (hashed,     _, _) =          hash lengths     (fromList [0..255], 0, 0)
        (sparseHash, _, _) = iterate (hash newLengths) (fromList [0..255], 0, 0) !! 64
        condensedHash = condense $ toList sparseHash
        hexString     = foldr (\digit str -> showHexPrepended digit $ str) "" condensedHash
    print $ (unsafeLookup 0 hashed) * (unsafeLookup 1 hashed)
    print $ hexString
