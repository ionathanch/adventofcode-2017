import Data.List.Split (splitOn)
import Data.Foldable (fold)

data Coordinates = Coordinates Int Int Int deriving Show
instance Monoid Coordinates where
    mempty = Coordinates 0 0 0
    Coordinates x y z `mappend` Coordinates x' y' z' = Coordinates (x + x') (y + y') (z + z')

getCoordinates :: String -> Coordinates
getCoordinates direction = case direction of
    "n"  -> Coordinates  1     0    0
    "ne" -> Coordinates  0     1    0
    "se" -> Coordinates  0     0    1
    "s"  -> Coordinates (-1)   0    0
    "sw" -> Coordinates  0   (-1)   0
    "nw" -> Coordinates  0     0  (-1)

getDistance :: Coordinates -> Int
getDistance (Coordinates x y z) =
    let absList = map abs [x, y, z]
    in  sum absList - minimum absList

main :: IO ()
main = do
    coordinates <- fmap (map getCoordinates . splitOn ",") $ readFile "11.txt"
    print $ getDistance $ fold coordinates
    print $ maximum . map getDistance $ scanl mappend mempty coordinates