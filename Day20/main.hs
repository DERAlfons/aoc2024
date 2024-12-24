import Data.List (find)
import Data.Array (
    Array, listArray, (//),
    bounds, inRange, indices, assocs, (!))
import Control.Monad (guard)

import My.Util (count)

type Point = (Int, Int)
type Direction = (Int, Int)

disk :: Int -> [Direction]
disk r = do
    dx <- [-r .. r]
    dy <- [-r + abs dx .. r - abs dx]
    return (dx, dy)

nbs :: Int -> Point -> [Point]
nbs r (x, y) = do
    (dx, dy) <- disk r
    guard $ (dx, dy) /= (0, 0)
    return (x + dx, y + dy)

incPath :: Array Point Int -> Point -> Array Point Int
incPath grid p = case find ((== 0) . (grid !)) $ nbs 1 p of
    Nothing -> grid
    Just nb -> incPath (grid // [(nb, (grid ! p) + 1)]) nb

shortcuts :: Int -> Array Point Int -> [Int]
shortcuts r grid = do
    p @ (x, y) <- indices grid
    guard $ grid ! p /= -1
    (dx, dy) <- disk r
    let dst = (x + dx, y + dy)
    guard $ inRange (bounds grid) dst
    guard $ grid ! dst /= -1
    return $ (grid ! dst) - (grid ! p) - abs dx - abs dy

cToI :: Char -> Int
cToI '#' = -1
cToI 'S' = 1
cToI _ = 0

main :: IO ()
main = do
    input <- lines <$> readFile "Day20/input.txt"
    let n = length input
        m = length $ input !! 0
        gridC = listArray ((0, 0), (n - 1, m - 1)) $ concat input
        Just start = fst <$> find ((== 'S') . snd) (assocs gridC)
        grid0 = cToI <$> gridC
        grid = incPath grid0 start
    print $ count (>= 100) $ shortcuts 2 grid
    print $ count (>= 100) $ shortcuts 20 grid
    return ()
