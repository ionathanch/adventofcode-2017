{-# LANGUAGE BangPatterns #-}
import Data.Function (on)
import Data.Bits ((.&.))

divisor = 2147483647
factors = (16807, 48271)
seed    = (634,   301)
        
count :: (Int, Int) -> (Int, Int) -> Int -> Int -> Int
count pair masks acc times =
    if times == 0 then acc else
    let !next = nextMaskBy <$$> factors <**> masks <**> pair
        !eq   = fromEnum $ uncurry ((==) `on` (.&. 0xffff)) next
    in count next masks (acc + eq) (times - 1)
    where
        h      <$$> (x, y) = (h x, h y)
        (f, g) <**> (x, y) = (f x, g y)
        nextMaskBy f m s  = let t = (f * s) `mod` divisor in if (t .&. m) == 0 then t else nextMaskBy f m t

main :: IO ()
main = do
    print $ count seed (0, 0) 0 40000000
    print $ count seed (3, 7) 0 5000000