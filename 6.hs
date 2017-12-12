import Data.Foldable
import Data.HashMap
import Data.Sequence

type Bank   = Seq Int
type Config = (Bank, Data.HashMap.Map [Int] Int)

(%) :: Int -> Int -> Int
(%) = mod

getMaxMem :: Bank -> (Int, Int)
getMaxMem bank =
    foldlWithIndex (\(currIndex, currMax) index value -> if value > currMax then (index, value) else (currIndex, currMax)) (0, 0) bank

nextBank :: Bank -> Bank
nextBank bank =
    let len = Data.Sequence.length bank
        (index, value) = getMaxMem bank
        zeroedBank = Data.Sequence.update index 0 bank
        mappedBank = fmap (+ value `div` len) zeroedBank
        indicesToUpdate = fmap (% len) [(index + 1)..(index + value % len)]
    in  mapWithIndex (\i v -> if i `elem` indicesToUpdate then v + 1 else v) mappedBank


cycles :: Int -> Config -> (Int, Int)
cycles count (prevBank, configs) =
    let bank = nextBank prevBank
        hashableBank    = Data.Foldable.toList bank
    in  if   member hashableBank configs
        then (count + 1, (count + 1) - findWithDefault undefined hashableBank configs)
        else cycles (count + 1) (bank, insert hashableBank (count + 1) configs)

main :: IO ()
main = do
    input <- readFile "6.txt"
    let bank = Data.Sequence.fromList $ fmap read $ words input :: Bank
    print $ cycles 0 (bank, Data.HashMap.empty)
