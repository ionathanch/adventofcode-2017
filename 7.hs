import Data.HashMap (Map, fromList, toList, delete, findWithDefault)
import Data.List.Split
import Data.Tree (Tree(..), unfoldTree, drawTree)

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

cumulate :: Tree Int -> Tree (Int, Int)
cumulate (Node root forest) =
    let newForest = map cumulate forest
        cumulativeWeight = foldr (\(Node (_, subWeight) _) weight -> weight + subWeight) 0 newForest
    in  Node { rootLabel = (root, cumulativeWeight + root), subForest = newForest }

findUnbalanced :: Tree (Int, Int) -> Int
findUnbalanced (Node _ forest) =
    let (diff, notMedian) = getDiffMedian $ fmap (\(Node (_, subWeight) _) -> subWeight) forest
        unbalanced = diff + foldr (\(Node (subRoot, subWeight) _) acc -> if subWeight == notMedian then subRoot else acc) 0 forest
        subUnbalanced = sum $ map findUnbalanced forest
    in  minOrZero unbalanced subUnbalanced

minOrZero :: Int -> Int -> Int
minOrZero 0 0 = 0
minOrZero x 0 = x
minOrZero 0 y = y
minOrZero x y = min x y

getDiffMedian :: [Int] -> (Int, Int)
getDiffMedian ns =
    let diffs = filter (/= 0) $ map (+ (- head ns)) ns
    in  case length diffs of
        0 -> (0, -1)
        1 -> (- head diffs, head ns + head diffs)
        _ -> (  head diffs, head ns)

main :: IO ()
main = do
    input <- readFile "7.txt"
    let programsList = map parseLine $ lines input
        programsMap = fromList programsList
        bottomName = getBottom programsMap programsList
        programsTree = mapToTree programsMap bottomName
        cumulateTree = cumulate programsTree
        unbalanced = findUnbalanced cumulateTree
    print $ bottomName
    writeFile "7_tree.txt" $ drawTree $ fmap show programsTree
    writeFile "7_cumulate.txt" $ drawTree $ fmap show cumulateTree
    print $ unbalanced