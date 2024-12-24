import Data.Set (Set, fromList, notMember)
import Data.Array (Array, listArray, (//), bounds, inRange, (!))
import Control.Monad (guard)

import Algorithm.Search (dijkstraAssoc)

import My.Parser (parserRegex, parserList, run)
import My.Util (maybeToIO)

type Point = (Int, Int)

step :: Array Point Char -> Point -> [(Point, Int)]
step grid (x, y) = do
    (dx, dy) <- [(0, -1), (0, 1), (-1, 0), (1, 0)]
    let nb = (x + dx, y + dy)
    guard $ inRange (bounds grid) nb
    guard $ grid ! nb == '.'
    return (nb, 1)

block :: Array Point Char -> Set Point -> [Point] -> Point
block grid path (p : ps) 
    | p `notMember` path = block gridNew path ps
    | otherwise = case dijkstraAssoc (step gridNew) (== end) start of
        Just (_, pathNew) -> block gridNew (fromList pathNew) ps
        Nothing -> p
    where
    (start, end) = bounds grid
    gridNew = grid // [(p, '#')]
 
parser :: String -> Maybe [Point]
parser = run $ parserList "" "\\n" "" $
    parserRegex "(\\d+),(\\d+)" $
        \ [x, y] -> (read x, read y)

main :: IO ()
main = do
    input <- maybeToIO "bla" . parser =<< readFile "Day18/input.txt"
    let start = (0, 0)
        end = (70, 70)
        gridE = listArray (start, end) (repeat '.')
        grid = gridE // zip (take 1024 input) (repeat '#')
        Just (pathLen, path) = dijkstraAssoc (step grid) (== end) start
    print pathLen
    print $ block grid (fromList path) (drop 1024 input)
    return ()
