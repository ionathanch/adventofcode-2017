{-# LANGUAGE BangPatterns #-}
(%)   = mod
steps = 312

postNth :: Int -> Int -> [Int] -> Int
postNth 2018 pos list = list !! (pos + 1)
postNth n    pos list =
    let !newPos  = (pos + steps % n + 1) % n
        !newList = take newPos list ++ [n] ++ drop newPos list
    in  postNth (n + 1) newPos newList

oneNth  :: Int -> Int -> Int -> Int
oneNth  50000001 _   oneth = oneth
oneNth  n pos !oneth =
    let !newPos   = (pos + steps % n + 1) % n
        !newOneth = if newPos == 0 then n else oneth
    in  oneNth (n + 1) newPos newOneth

main :: IO ()
main = do
    print $ postNth 1 1 [0]
    print $ oneNth  1 1 1