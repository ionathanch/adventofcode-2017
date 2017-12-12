import Data.Foldable (toList)
import Data.HashMap (Map, member, insert, findWithDefault, empty)
import Data.Sequence (Seq, length, update, fromList, foldlWithIndex, mapWithIndex)

type Bank   = Seq Int
type HashableBank = [Int]
type Config = (Bank, Map HashableBank Int)

(%) :: Int -> Int -> Int
(%) = mod

(//) :: Int -> Int -> Int
(//) = div

getMaxMem :: Bank -> (Int, Int)
getMaxMem bank =
    foldlWithIndex (\(currIndex, currMax) index value -> if value > currMax then (index, value) else (currIndex, currMax)) (0, 0) bank

nextBank :: Bank -> Bank
nextBank bank =
    let len = Data.Sequence.length bank
        (index, value) = getMaxMem bank
        zeroedBank = update index 0 bank
        mappedBank = fmap (+ value // len) zeroedBank
        indicesToUpdate = fmap ((% len) . (+ index)) [1..value % len]
    in  mapWithIndex (\i v -> if i `elem` indicesToUpdate then v + 1 else v) mappedBank


cycles :: Int -> Config -> (Int, Int)
cycles prevCount (prevBank, banks) =
    let count = prevCount + 1
        bank = nextBank prevBank
        hashableBank = toList bank
    in  if   member hashableBank banks
        then (count, count - findWithDefault undefined hashableBank banks)
        else cycles count (bank, insert hashableBank count banks)

main :: IO ()
main = do
    input <- readFile "6.txt"
    let bank = fromList $ fmap read $ words input :: Bank
    print $ cycles 0 (bank, empty)
