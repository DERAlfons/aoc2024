import Data.List (find)
import Data.Set (empty, insert, member, size)
import qualified Data.Set as S
import Data.Array (Array, listArray, (//), (!), assocs)

import Day16.Search (dijkstraAssoc, QList (..))

type Point = (Int, Int)
type Direction = (Int, Int)
type Configuration = (Point, Direction)

step :: Array Point Char -> Configuration -> [(Configuration, Int)]
step maze ((x, y), (dx, dy)) = let pStep = (x + dx, y + dy) in
    [((pStep, (dx, dy)), 1) | maze ! pStep == '.'] ++
    [(((x, y), (dy, -dx)), 1000), (((x, y), (-dy, dx)), 1000)]

points :: QList Configuration -> Int
points = size . S.map fst . loop empty
    where
    loop acc S = acc
    loop acc (D a rest)
        | a `member` acc = acc
        | otherwise = loop (insert a acc) rest
    loop acc (Q q1 q2) = loop (loop acc q1) q2

main :: IO ()
main = do
    input <- lines <$> readFile "Day16/input.txt"
    let n = length input
        m = length $ input !! 0
        mazeT = listArray ((0, 0), (n - 1, m - 1)) $ concat input
        Just startP = fst <$> find ((== 'S') . snd) (assocs mazeT)
        Just endP = fst <$> find ((== 'E') . snd) (assocs mazeT)
        maze = mazeT // [(startP, '.'), (endP, '.')]
        start = (startP, (0, 1))
    let Just (cost, path) = dijkstraAssoc (step maze) ((== endP) . fst) start
    print cost
    print $ points $ D start path
    return ()
