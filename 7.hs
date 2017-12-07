import Data.HashMap (Map, fromList, toList, delete, findWithDefault)
import Data.List.Split
import Data.Tree (Tree, unfoldTree, drawTree)

type Weight = Int
type Program = String
type Programs = [Program]

discardEmpty :: [String] -> [String]
discardEmpty [""] = []
discardEmpty xs = xs

parseLine :: String -> (Program, (Weight, Programs))
parseLine line =
    let nameAndWeight : programsString : _ = splitOn ")" line
        programs = discardEmpty $ splitOn ", " $ last $ splitOn " -> " programsString
        name : weight : _ = splitOn " (" nameAndWeight
    in (name, (read weight, programs))

getBottom :: Map Program (Weight, Programs) -> [(Program, (Weight, Programs))] -> Program
getBottom m l = bottomName
    where 
        (bottomName, _) : _ = toList $ foldr 
            (\(name, (_, programs)) set -> 
                case programs of
                    [] -> delete name set
                    ps -> foldr delete set ps) 
            m l

mapToTree :: Map Program (Weight, Programs) -> Program -> Tree Weight
mapToTree m = unfoldTree (\s -> findWithDefault undefined s m)

main :: IO ()
main = do
    input <- readFile "7.txt"
    let programsList = map parseLine $ lines input
        programsMap = fromList programsList
        bottomName = getBottom programsMap programsList
        programsTree = mapToTree programsMap bottomName
    print $ bottomName
    writeFile "7_tree.txt" $ drawTree $ fmap show programsTree