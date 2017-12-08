import Data.HashMap (Map, insert, alter, empty, toList, findWithDefault)
import qualified Data.HashMap as M (lookup)

data Instruction = I {
    reg :: String,
    inc :: Int,
    src :: String,
    fnc :: Int -> Bool
}

getFnc :: String -> (Int -> Int -> Bool)
getFnc s = case s of 
    "<"  -> (<)
    "<=" -> (<=)
    ">"  -> (>)
    ">=" -> (>=)
    "==" -> (==)
    "!=" -> (/=)

getOrSetZero :: String -> Map String Int -> (Int, Map String Int)
getOrSetZero s m =
    case M.lookup s m of
        Just value -> (value, m)
        Nothing    -> (0,     insert s 0 m)

addToMaybe :: Int -> Maybe Int -> Maybe Int
addToMaybe i (Just x) = Just $ i + x
addToMaybe i Nothing  = Just i

parseLine :: String -> Instruction
parseLine s =
    let (register : command : value : _ : source : function : argument : _) = words s
    in I {
        reg = register,
        inc = case command of
            "inc" ->    read value
            "dec" -> (- read value),
        src = source,
        fnc = flip (getFnc function) $ read argument
    }

executeInstruction :: (Map String Int, Int) -> Instruction -> (Map String Int, Int)
executeInstruction (m, highest) (I r i s f) =
    let (value, newMap) = getOrSetZero s m
        newHighest = max highest $ findWithDefault highest r newMap
    in  if f value
        then (alter (addToMaybe i) r newMap, newHighest)
        else (newMap, highest)

main :: IO ()
main = do
    input <- readFile "8.txt"
    let instructions = map parseLine $ lines input
        (finalMap, highest) = foldl executeInstruction (empty, 0) instructions
        maxValue = maximum . snd . unzip . toList $ finalMap
    print $ maxValue
    print $ highest