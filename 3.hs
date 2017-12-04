import qualified Data.Map as Map

squareRoot :: Floating a => Int -> a
squareRoot = sqrt . fromIntegral

getLevel :: Int -> Int
getLevel n = ceiling $ (squareRoot n - 1) / 2

getDownstairs :: Int -> Int
getDownstairs n =
    let level = getLevel n
    in  n - (8 * level - 7 + 2 * ((n - (2 * level - 1) ^ 2) `div` (2 * level)))

type Store = Map.Map Int Int

getValue :: Int -> Store -> Int
getValue k m = Map.findWithDefault undefined k m 
    
computeValue :: Int -> Store -> Store
computeValue n m = Map.insert n value m
    where 
        level          = getLevel n
        downstairs     = getDownstairs n
        preDownstairs  = getDownstairs (n-1)
        postDownstairs = getDownstairs (n+1)
        lastCorner = (2 * level + 1) ^ 2
        firstPost  = (2 * level - 1) ^ 2 + 1
        secondPost = firstPost + 1
        corners    = map ((+ lastCorner) . (*(2 * level))) [(-3)..(-1)]
        pre        = map (+(-1)) corners
        post       = map (+1) (lastCorner:corners)
        value
            | n == firstPost    = (getValue (n-1) m) + (getValue postDownstairs m)
            | n == lastCorner   = (getValue (n-1) m) + (getValue preDownstairs  m) + (getValue (preDownstairs+1) m)
            | n `elem` corners  = (getValue (n-1) m) + (getValue preDownstairs  m)
            | n `elem` pre      = (getValue (n-1) m) + (getValue (downstairs-1) m) + (getValue downstairs m) 
            | n `elem` post || 
              n == secondPost   = (getValue (n-1) m) + (getValue (n-2)          m) + (getValue downstairs m) + (getValue (downstairs+1) m)
            | otherwise         = (getValue (n-1) m) + (getValue (downstairs-1) m) + (getValue downstairs m) + (getValue (downstairs+1) m)

computeUntil :: Int -> Int -> Store -> Int
computeUntil max n m =
    let newStore = computeValue n m
        value = getValue n newStore
    in  if value < max
        then computeUntil max (n+1) newStore
        else value

main :: IO ()
main = do
    let initialStore = Map.fromList $ zip [1..9] [1, 1, 2, 4, 5, 10, 11, 23, 25]
    print $ computeUntil 368078 10 initialStore
        