import Data.HashMap (Map, alter, empty, findWithDefault, elems)

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

executeInstruction :: Map String Int -> Instruction -> Map String Int
executeInstruction m (I r v s f) =
    alter (addToMaybe . (*v) . fromEnum . f $ findWithDefault 0 s m) r m

main :: IO ()
main = do
    maxima <- map (maximum . elems) . tail . scanl executeInstruction empty . map parseLine . lines <$> readFile "08.txt"
    print $ last maxima
    print $ maximum maxima