import Data.HashMap (Map, insert, alter, empty, toList)
import qualified Data.HashMap as M (lookup)
import Debug.Trace

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

executeInstruction :: Map String Int -> Instruction -> Map String Int
executeInstruction m (I r i s f) =
    let (value, newMap) = getOrSetZero s m
    in  if f value
        then alter (addToMaybe i) r newMap
        else newMap

main :: IO ()
main = do
    input <- readFile "8.txt"
    let instructions = map parseLine $ lines input
        finalMap = foldl executeInstruction empty instructions
        maxValue = maximum . snd . unzip . toList $ finalMap
    print $ maxValue