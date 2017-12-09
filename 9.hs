
removeCancelled :: String -> String
removeCancelled str =
    let (_, removed) = foldl (\(prev, rs) curr -> 
            case prev of 
                '!'  -> ('\0',        rs)
                '\0' -> (curr,        rs)
                _    -> (curr, prev : rs))
            (head str, "") (tail str)
    in  reverse $ filter (/= '\0') removed

removeGarbage :: String -> String
removeGarbage str =
    let (_, removed) = foldl (\(isGarbage, rs) curr ->
            if   isGarbage
            then (curr /= '>', rs)
            else (curr == '<',
                if   curr == '<' 
                then rs 
                else curr : rs))
            (False, "") str
    in  reverse $ filter (/= ',') removed

countNonGarbage :: String -> Int
countNonGarbage str =
    let (_, removed) = foldl (\(isGarbage, rs) curr ->
            if   isGarbage
            then (curr /= '>', 
                if   curr == '>'
                then rs
                else curr : rs)
            else (curr == '<', rs))
            (False, "") str
    in  length removed

countGroups :: String -> Int
countGroups str =
    let (_, total) = foldl (\(score, acc) curr -> 
            case curr of
                '{' -> (score + 1, acc + score)
                '}' -> (score - 1, acc)) 
            (1, 0) str
    in total

main :: IO ()
main = do
    input <- readFile "9.txt"
    print $ countGroups . removeGarbage . removeCancelled $ input
    print $ countNonGarbage . removeCancelled $ input