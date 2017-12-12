import Data.List
import Data.Set

isPassphraseValid :: [String] -> Bool
isPassphraseValid ws = sort ws == (toAscList . fromList) ws

main :: IO ()
main = do
    input <- readFile "4.txt"
    let passphrases = fmap words $ lines input
    let valids = sum $ fmap (fromEnum . isPassphraseValid) passphrases
    print $ valids
    let stillValids = sum $ fmap (fromEnum . isPassphraseValid . (fmap sort)) passphrases
    print $ stillValids