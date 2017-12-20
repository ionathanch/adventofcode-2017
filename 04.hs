import Data.List (sort)
import Data.Set (toAscList, fromList)

isPassphraseValid :: [String] -> Bool
isPassphraseValid ws = sort ws == (toAscList . fromList) ws

main :: IO ()
main = do
    passphrases <- map words . lines <$> readFile "04.txt"
    let valids      = sum $ map (fromEnum . isPassphraseValid) passphrases
    let stillValids = sum $ map (fromEnum . isPassphraseValid . (map sort)) passphrases
    print $ valids
    print $ stillValids