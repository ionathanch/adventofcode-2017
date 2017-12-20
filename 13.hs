import Data.List.Split (splitOn)
import Data.List (findIndex)

type Delay = Int
type Firewall = (Int, Int)
(%) = mod

parseLine :: String -> Firewall
parseLine str =
    let depth : range : _ = splitOn ": " str
    in  (read depth, read range)

caught :: Delay -> Firewall -> Bool
caught delay (depth, range) =
    (depth + delay) % (2 * (range - 1)) == 0

severity :: [Firewall] -> Int
severity firewalls =
    sum . map (\firewall -> uncurry (*) firewall * fromEnum (caught 0 firewall)) $ firewalls

anyCaught :: [Firewall] -> Delay -> Bool
anyCaught firewalls delay =
    or  . map (caught delay) $ firewalls

main :: IO ()
main = do
    firewalls <- map parseLine . lines <$> readFile "13.txt"
    print $ severity firewalls
    print $ findIndex not $ map (anyCaught firewalls) [0..]