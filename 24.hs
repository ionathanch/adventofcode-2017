import Data.List.Split (splitOn)
import Data.Sequence (Seq, (!?), (<|), fromList, empty, findIndicesL, deleteAt)

type Port = Int
type Component = (Port, Port)
type Bridge = Seq Component
type Est = Bridge -> (Int, Bridge) -> (Int, Bridge)

bridgeStrength :: Bridge -> Int
bridgeStrength components = sum . fmap (uncurry (+)) $ components

strongest :: Est
strongest bridge s@(currStrength, _) =
    if bridgeStrength bridge > currStrength then (bridgeStrength bridge, bridge) else s

longest :: Est
longest bridge l@(currLength, _) =
    if length bridge > currLength then (length bridge, bridge) else l

estBridge :: Est -> Port -> Bridge -> Bridge
estBridge est port components =
    let indicesOfPort = findIndicesL (\(a, b) -> a == port || b == port) components
        subbridges    = map subbridge indicesOfPort
    in  snd $ foldr est (0, empty) subbridges
    where subbridge i =
            let Just c@(a, b) = components !? i
            in  c <| estBridge est (if a == port then b else a) (deleteAt i components)

main :: IO ()
main = do
    components <- fromList . map (\line -> let a : b : [] = splitOn "/" line in (read a, read b)) . lines <$> readFile "24.txt"
    print $ bridgeStrength $ estBridge strongest 0 components
    print $ bridgeStrength $ estBridge longest   0 components