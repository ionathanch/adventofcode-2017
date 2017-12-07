import Data.HashSet (Set, fromList, delete)
import Data.List.Split
import Debug.Trace

type Weight = Int
type Program = String
type Programs = [Program]
--type Tree = Map Program (Weight, Programs)

discardEmpty :: [String] -> [String]
discardEmpty [""] = []
discardEmpty xs = xs

parseLine :: String -> (Program, Weight, Programs)
parseLine line =
    let nameAndWeight : programsString : _ = splitOn ")" line
        programs = discardEmpty $ splitOn ", " $ last $ splitOn " -> " programsString
        name : weight : _ = splitOn " (" nameAndWeight
    in (name, read weight, programs)

main :: IO ()
main = do
    input <- readFile "7.txt"
    let list  = map parseLine $ lines input
        names = fromList $ map (\(name, _, _) -> name) list
        bottom = foldr (\(name, _, programs) set -> 
            case programs of
                [] -> delete name set
                ps -> foldr delete set ps)
            names list
    print $ bottom