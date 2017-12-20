import Data.List.Split (splitOn)
import Data.List (elemIndex, sortOn, groupBy)
import Data.Monoid ((<>))
import Data.Function (on)

data Property = Property Int Int Int deriving (Eq, Ord)
data Particle = Particle {
    position     :: Property,
    velocity     :: Property,
    acceleration :: Property
}

instance Monoid Property where
    mempty  = Property 0 0 0
    Property x1 y1 z1 `mappend` Property x2 y2 z2 = Property (x1 + x2) (y1 + y2) (z1 + z2)

distance :: Particle -> Int
distance (Particle (Property x y z) _ _) = abs x + abs y + abs z

updateParticle :: Int -> Particle -> Particle
updateParticle t (Particle p v a) =
    Particle (p <> t *** v <> (t * t `div` 2) *** a) (v <> t *** a) a
    where n *** (Property x y z) = Property (n * x) (n * y) (n * z)

stepParticles :: [Particle] -> [Particle]
stepParticles particles =
    concat . filter ((== 1) . length) . groupBy ((==) `on` position) . sortOn position . map step $ particles
    where step (Particle p v a) = Particle (p <> v <> a) (v <> a) a

parseProperty :: String -> Property
parseProperty str = 
    let x : y : z : [] = map read . splitOn "," . drop 3 . init $ str
    in Property x y z

parseLine :: String -> Particle
parseLine str =
    let p : v : a : [] = map parseProperty . splitOn ", " $ str
    in Particle p v a

main :: IO ()
main = do
    particles <- map parseLine . lines <$> readFile "20.txt"
    let distances = map (distance . updateParticle 400) particles
    print $ elemIndex (minimum distances) distances
    print $ length $ iterate stepParticles particles !! 40