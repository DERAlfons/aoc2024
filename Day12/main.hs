import qualified Data.Set as S
import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Monad (guard)

import My.Util (for)

type Point = (Int, Int)
type Direction = (Int, Int)

nesw :: [Direction]
nesw = [(0, -1), (0, 1), (-1, 0), (1, 0)]

step :: Point -> Direction -> Point
step (i, j) (di, dj) = (i + di, j + dj)

block :: Eq a => [Point] -> [Point] -> Map Point a -> ([Point], Map Point a)
block acc [] m = (acc, m)
block acc (p : ps) m
    | p `M.notMember` m = block acc ps m
    | otherwise = let
        nbs = do
            nb <- (p `step`) <$> nesw
            guard $ nb `M.member` m
            guard $ m ! nb == m ! p
            return nb in
        block (p : acc) (nbs ++ ps) (M.delete p m)

getRegions :: Eq a => Map Point a -> [[Point]]
getRegions m
    | M.null m = []
    | otherwise = let
        (p, _) = M.findMin m
        (b, mNew) = block [] [p] m in
        b : getRegions mNew

borders :: [Point] -> [[Point]]
borders ps = for nesw $ \ d ->
    filter ((`S.notMember` S.fromList ps) . (`step` d)) ps

perimeter :: [Point] -> Int
perimeter ps = sum $ length <$> borders ps

sides :: [Point] -> Int
sides ps = sum $ do
    b <- borders ps
    return $ length $ getRegions $ M.fromList $ zip b [0, 0 ..]

main :: IO ()
main = do
    grid <- lines <$> readFile "Day12/input.txt"
    let gridMap = M.fromList $ do
            (i, row) <- zip [0 ..] grid
            (j, x) <- zip [0 ..] row
            return ((i, j), x)
        regions = getRegions gridMap
    print $ sum $ ((*) <$> perimeter <*> length) <$> regions
    print $ sum $ ((*) <$> sides <*> length) <$> regions
    return ()
