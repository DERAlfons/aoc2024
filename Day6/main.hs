import Data.List (find, (\\))
import Data.Set (Set, singleton, insert, member, toList)
import qualified Data.Set as S
import Data.Array (
    Array, listArray, (//), (!),
    bounds, inRange, indices, assocs)

import My.Util (count, for)

type Point = (Int, Int)
type Direction = (Int, Int)
type Configuration = (Point, Direction)
data Path = Path {configurations :: Set Configuration, loop :: Bool}

getPath :: Array Point Char -> Configuration -> Path
getPath grid start = loop (singleton start) start
    where
    loop :: Set Configuration -> Configuration -> Path
    loop path (p @ (i, j), d @ (di, dj))
        | not $ inRange (bounds grid) nxt = Path path False
        | (nxt, d) `member` path = Path path True
        | grid ! nxt == '#' = loop path (p, (dj, -di))
        | otherwise = loop (insert (nxt, d) path) (nxt, d)
        where
        nxt = (i + di, j + dj)

main :: IO ()
main = do
    input <- lines <$> readFile "Day6/input.txt"
    let n = length input
        m = length $ input !! 0
        grid = listArray ((0, 0), (n - 1, m - 1)) $ concat input
        Just startP = fst <$> find ((== '^') . snd) (assocs grid)
        start = (startP, (-1, 0))
        points = toList $ S.map fst $ configurations $ getPath grid start
    print $ length points
    print $ count loop $ for (points \\ [startP])
        (\ p -> getPath (grid // [(p, '#')]) start)
    return ()
