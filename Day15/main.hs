import Data.Maybe (fromMaybe)
import Data.List (find, foldl')
import Data.Array (Array, listArray, (//), (!), assocs)
import My.Util (explode)

type Point = (Int, Int)
type Direction = (Int, Int)

d :: Char -> Direction
d '<' = ( 0, -1)
d '>' = ( 0,  1)
d '^' = (-1,  0)
d 'v' = ( 1,  0)

push :: Direction -> (Array Point Char, Point, Point, Char) -> Maybe (Array Point Char, Point, Point, Char)
push (dx, dy) (grid, (rx, ry), (x, y), c) = case grid ! (x, y) of
    '#' -> Nothing
    '.' -> Just (grid // [((x, y), c)], rNew, (x, y), '.')
    'O' -> push (dx, dy) (grid // [((x, y), c)], rNew, (x + dx, y + dy), 'O')
    where
    rNew = if c == '.' then (rx + dx, ry + dy) else (rx, ry)

step :: (Array Point Char, Point) -> Char -> (Array Point Char, Point)
step (grid, (x, y)) c = let
    (dx, dy) = d c
    (gridNew, r, _, _) = fromMaybe <*> push (d c) $ (grid, (x, y), (x + dx, y + dy), '.') in
    (gridNew, r)

main :: IO ()
main = do
    [gridS, moves] <- explode "" . lines <$> readFile "Day15/input.txt"
    let n = length gridS
        m = length $ gridS !! 0
        gridArr = listArray ((0, 0), (n - 1, m - 1)) $ concat gridS
        Just start = fst <$> find ((== '@') . snd) (assocs gridArr)
        grid = gridArr // [(start, '.')]
        (gridEnd, _) = foldl' step (grid, start) $ concat moves
    print $ sum $ map (\ ((i, j), _) -> i * 100 + j) $ filter ((== 'O') . snd) (assocs gridEnd)
    return ()
