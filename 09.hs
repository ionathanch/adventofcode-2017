scoreAndCount :: String -> (Int, Int)
scoreAndCount str =
    let (_, _, _, score, count) = foldl f (False, False, 1, 0, 0) str
    in  (score, count)
    where f (isCancel, isGarbage, level, score, count) curr
            | isCancel    = (False,       isGarbage,   level,     score,         count)
            | isGarbage   = (curr == '!', curr /= '>', level,     score,         count + (fromEnum $ curr /= '>' && curr /= '!'))
            | curr == '{' = (False,       False,       level + 1, score + level, count)
            | curr == '}' = (False,       False,       level - 1, score,         count)
            | curr == ',' = (False,       False,       level,     score,         count)
            | curr == '<' = (False,       True,        level,     score,         count)

main :: IO ()
main = do
    readFile "09.txt" >>= print . scoreAndCount