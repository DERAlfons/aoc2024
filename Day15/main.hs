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

push :: Direction -> (Array Point Char, Point) -> Maybe (Array Point Char, Point)
push (dx, dy) (grid, (x, y)) = let
    pNew = (x + dx, y + dy) in
    flip (,) pNew <$> loop pNew '.' grid
    where
    loop (x, y) c grid = case grid ! (x, y) of
        '#' -> Nothing
        '.' -> Just $ grid // [((x, y), c)]
        'O' -> loop (x + dx, y + dy) 'O' $ grid // [((x, y), c)]
        '[' -> loop (x + dx, y + dy + 1) ']' =<< 
            loop (x + dx, y + dy) '[' (
            grid // [((x, y), c), ((x, y + 1), '.')])
        ']' -> loop (x + dx, y + dy - 1) '[' =<<
            loop (x + dx, y + dy) ']' (
            grid // [((x, y), c), ((x, y - 1), '.')])

step :: (Array Point Char, Point) -> Char -> (Array Point Char, Point)
step gridPos c = fromMaybe <*> push (d c) $ gridPos

expand :: Char -> String
expand '#' = "##"
expand 'O' = "[]"
expand '.' = ".."
expand '@' = "@."

main :: IO ()
main = do
    [gridT, moves] <- explode "" . lines <$> readFile "Day15/input.txt"
    sequence $ do
        gridS <- [gridT, map (expand =<<) gridT]
        let n = length gridS
            m = length $ gridS !! 0
            gridArr = listArray ((0, 0), (n - 1, m - 1)) $ concat gridS
            Just start = fst <$> find ((== '@') . snd) (assocs gridArr)
            grid = gridArr // [(start, '.')]
            (gridEnd, _) = foldl' step (grid, start) $ concat moves
        return $ print $ sum $ map (\ (i, j) -> i * 100 + j) $ fst <$> filter (\ (_, b) -> b == 'O' || b == '[') (assocs gridEnd)
    return ()
