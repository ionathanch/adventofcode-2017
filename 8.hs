import Data.HashMap (Map, alter, empty, toList, findWithDefault)

data Instruction = I {
    reg :: String,
    val :: Int,
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

addToMaybe :: Int -> Maybe Int -> Maybe Int
addToMaybe i (Just x) = Just $ i + x
addToMaybe i Nothing  = Just i

parseLine :: String -> Instruction
parseLine s =
    let (register : operation : value : _ : source : function : argument : _) = words s
    in I {
        reg = register,
        val = case operation of
            "inc" ->    read value
            "dec" -> (- read value),
        src = source,
        fnc = flip (getFnc function) $ read argument
    }

executeInstruction :: (Map String Int, Int) -> Instruction -> (Map String Int, Int)
executeInstruction (m, highest) (I r v s f) =
    let value      = findWithDefault 0 s m
        newHighest = max highest $ findWithDefault highest r m
    in  if   f value
        then (alter (addToMaybe v) r m, newHighest)
        else (m, highest)

main :: IO ()
main = do
    input <- readFile "8.txt"
    let instructions = map parseLine $ lines input
        (finalMap, highest) = foldl executeInstruction (empty, 0) instructions
        maxValue = maximum . snd . unzip . toList $ finalMap
    print $ maxValue
    print $ highest