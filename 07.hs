import Data.HashMap (Map, fromList, toList, delete, findWithDefault)
import Data.List.Split
import Data.Tree (Tree(..), Forest, unfoldTree, drawTree)

type Weight   = Int
type Program  = String
type Programs = [Program]

discardEmpty :: [String] -> [String]
discardEmpty [""] = []
discardEmpty xs   = xs

minOrZero :: Int -> Int -> Int
minOrZero x 0 = x
minOrZero 0 y = y
minOrZero x y = min x y

getWeights :: Forest (Weight, Int) -> [Weight]
getWeights = fmap (\(Node (_, weight) _) -> weight)

findRootWithWeight :: Int -> Forest (Weight, Int) -> Weight
findRootWithWeight w = foldr (\(Node (root, weight) _) acc -> if weight == w then root else acc) 0

-- input:  [Weight > 0] where all weights are the same except one
-- output: (difference between the median weight and the minority weight, the minority weight)
getDiffMinority :: [Int] -> (Int, Int)
getDiffMinority ns =
    let diffs = filter (/= 0) $ map (+ (- head ns)) ns
    in  case length diffs of
        0 -> (0, -1)
        1 -> (- head diffs, head ns + head diffs)
        _ -> (  head diffs, head ns)

parseLine :: String -> (Program, (Weight, Programs))
parseLine line =
    let nameAndWeight : programsString : _ = splitOn ")"  line
        name          : weight         : _ = splitOn " (" nameAndWeight
        programs = discardEmpty . splitOn ", " . last . splitOn " -> " $ programsString
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

cumulate :: Tree Weight -> Tree (Weight, Int)
cumulate (Node root forest) =
    let newForest        = map cumulate forest
        cumulativeWeight = (+ root) . sum . getWeights $ newForest
    in  Node { rootLabel = (root, cumulativeWeight), subForest = newForest }

findBalanced :: Tree (Weight, Int) -> Int
findBalanced (Node _ forest) =
    let (diff, minority) = getDiffMinority $ getWeights forest
        balanced         = diff + findRootWithWeight minority forest
        subBalanced      = sum $ map findBalanced forest
    in  minOrZero balanced subBalanced
        
main :: IO ()
main = do
    programsList <- fmap (map parseLine . lines) $ readFile "07.txt"
    let programsMap  = fromList  programsList
        bottomName   = getBottom programsMap programsList
        balanced     = findBalanced . cumulate $ mapToTree programsMap bottomName
        -- programsTree = mapToTree programsMap bottomName
        -- cumulateTree = cumulate  programsTree
    print $ bottomName
    print $ balanced
    -- writeFile "7_tree.txt"     $ drawTree $ fmap show programsTree
    -- writeFile "7_cumulate.txt" $ drawTree $ fmap show cumulateTree