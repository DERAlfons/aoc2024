import Data.Map (Map, singleton, fromListWith, size)
import qualified Data.Map as M
import Data.Array (Array, listArray, (!), bounds, inRange)
import qualified Data.Array as A
import Control.Monad (guard)

import My.Util (applyN)

step :: Array (Int, Int) Char -> Map (Int, Int) Int -> Map (Int, Int) Int
step grid points = fromListWith (+) $ do
    ((i, j), c) <- M.assocs points
    (ni, nj) <- [(i, j - 1), (i, j + 1), (i - 1, j), (i + 1, j)]
    guard $ inRange (bounds grid) (ni, nj)
    guard $ grid ! (ni, nj) == succ (grid ! (i, j))
    return ((ni, nj), c)

main :: IO ()
main = do
    input <- lines <$> readFile "Day10/input.txt"
    let n = length input
        m = length $ input !! 0
        grid = listArray ((0, 0), (n - 1, m - 1)) $ concat input
        start = flip singleton 1 . fst <$> filter ((== '0') . snd) (A.assocs grid)
        end = applyN 9 (step grid) <$> start
    print $ sum $ size <$> end
    print $ sum $ sum <$> end
    return ()
