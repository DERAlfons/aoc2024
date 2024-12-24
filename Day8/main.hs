import Data.Set (fromList, size)
import Data.Map (fromListWith, elems)
import Data.Array (inRange)
import Control.Monad (guard)

type Point = (Int, Int)
type Rectangle = (Point, Point)
type InterferenceScheme = Point -> Point -> [Point]

addP :: Point -> Point -> Point
addP (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

iScheme :: Rectangle -> Bool -> InterferenceScheme
iScheme bounds harmonics p1 @ (x1, y1) p2 @ (x2, y2)
    | p1 == p2 = []
    | harmonics =
        (takeWhile (inRange bounds) $ iterate (addP ( dx,  dy)) p2) ++
        (takeWhile (inRange bounds) $ iterate (addP (-dx, -dy)) p1)
    | otherwise = filter (inRange bounds) [addP p2 (dx, dy), addP p1 (-dx, -dy)]
    where
    (dx, dy) = (x2 - x1, y2 - y1)

getAntinodes :: InterferenceScheme -> [Point] -> [Point]
getAntinodes scheme ps = concat $ scheme <$> ps <*> ps

main :: IO ()
main = do
    input <- lines <$> readFile "Day8/input.txt"
    let n = length input
        m = length $ input !! 0
        bounds = ((0, 0), (n - 1, m - 1))
    let antennae = elems $ fromListWith (++) $ do
            (row, i) <- zip input [0 ..]
            (c, j) <- zip row [0 ..]
            guard $ c /= '.'
            return (c, [(i, j)])
    let antinodes = getAntinodes (iScheme bounds False) =<< antennae
    print $ size $ fromList antinodes
    let antinodesHarmonics = getAntinodes (iScheme bounds True) =<< antennae
    print $ size $ fromList antinodesHarmonics
    return ()
