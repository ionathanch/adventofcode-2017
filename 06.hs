import Data.Foldable (toList)
import Data.Maybe
import Data.HashMap (Map, member, insert, findWithDefault, empty)
import Data.Sequence (Seq, update, fromList, elemIndexL, mapWithIndex)
import qualified Data.Sequence as S (length)

type Bank = Seq Int
type Config = (Bank, Map [Int] Int)
(%) = mod
(//) = div

getMaxMem :: Bank -> (Int, Int)
getMaxMem bank =
    (fromJust $ elemIndexL (maximum bank) bank, maximum bank)

nextBank :: Bank -> Bank
nextBank bank =
    let len = S.length bank
        (index, value) = getMaxMem bank
        newBank = fmap (+ value // len) $ update index 0 bank
        indices = fmap (% len) [index + 1..index + value % len]
    in  mapWithIndex (\i v -> v + fromEnum (i `elem` indices)) newBank


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
    bank <- fromList . map read . words <$> readFile "06.txt"
    print $ cycles 0 (bank, empty)
