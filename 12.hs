import Data.List.Split (splitOn)
import Data.IntMap (IntMap, findWithDefault)
import Data.IntSet (IntSet, member, notMember, insert, delete, empty, size, findMin)
import qualified Data.IntMap as M (fromList)
import qualified Data.IntSet as S (fromList, null)

parseLine :: String -> (Int, [Int])
parseLine str =
    let src : dests : [] = splitOn " <-> " str
    in  (read src, map read $ splitOn ", " dests)

visit  :: IntMap [Int] -> Int -> IntSet -> IntSet
visit hashmap node hashset =
    let neighbours = filter (`notMember` hashset) $ findWithDefault [] node hashmap
    in  foldr (visit hashmap) (foldr insert hashset neighbours) neighbours

remove :: IntMap [Int] -> Int -> IntSet -> IntSet
remove hashmap node hashset =
    let neighbours = filter (`member` hashset) $ findWithDefault [] node hashmap
    in  foldr (remove hashmap) (foldr delete hashset neighbours) neighbours

countGroups :: IntMap [Int] -> Int -> IntSet -> Int
countGroups hashmap count hashset = if S.null hashset then count else
    countGroups hashmap (count + 1) $ remove hashmap (findMin hashset) hashset

main :: IO ()
main = do
    keyvals <- fmap (map parseLine . lines) $ readFile "12.txt"
    let hashkeys = S.fromList . fst . unzip $ keyvals
        hashmap  = M.fromList keyvals
    print $ size $ visit hashmap 0 empty
    print $ countGroups  hashmap 0 hashkeys