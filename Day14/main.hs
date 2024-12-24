import Data.List (intercalate)
import Data.Array (listArray, (//), (!), inRange)

import My.Util (maybeToIO)
import My.Parser (parserRegex, parserList, run)

type Point = (Int, Int)
type Direction = (Int, Int)

euclid :: Int -> Int -> (Int, Int)
euclid a b = loop a b 1 0 0 1
    where
    loop _ 0 s _ t _ = (s, t)
    loop r1 r2 s1 s2 t1 t2 = let
        (q, r) = quotRem r1 r2 in
        loop r2 r s2 (s1 - q * s2) t2 (t1 - q * t2)

step :: (Int, Int) -> Int -> Point -> Direction -> Point
step (bx, by) count (x, y) (dx, dy) =
    ((x + count * dx) `mod` bx, (y + count * dy) `mod` by)

parser :: String -> Maybe [(Point, Direction)]
parser = run $ parserList "" "\\n" "" $
    parserRegex "p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)" $
        \ [px, py, dx, dy] -> ((read px, read py), (read dx, read dy))

picture :: (Int, Int) -> [Point] -> String
picture (bx, by) ps = let
    arr = listArray ((0, 0), (bx - 1, by - 1)) (repeat ' ') // zip ps (repeat '#') in
    intercalate "\n" [[arr ! (i, j) | j <- [0 .. by - 1]] | i <- [0 .. bx - 1]]
    
main :: IO ()
main = do
    input <- maybeToIO "bla" . parser =<< readFile "Day14/input.txt"
    let (positions, velocities) = unzip input
        (bx, by) = (101, 103)
        endPositions = zipWith (step (bx, by) 100) positions velocities
        q1 = filter (inRange ((0, 0), (bx `div` 2 - 1, by `div` 2 - 1))) endPositions
        q2 = filter (inRange ((0, by `div` 2 + 1), (bx `div` 2 - 1, by))) endPositions
        q3 = filter (inRange ((bx `div` 2 + 1, 0), (bx, by `div` 2 - 1))) endPositions
        q4 = filter (inRange ((bx `div` 2 + 1, by `div` 2 + 1), (bx, by))) endPositions
    print $ product $ length <$> [q1, q2, q3, q4]
    sequence $ do
        c <- [40, 99, 8280]
        [print c, putStrLn $ picture (bx, by) $ zipWith (step (bx, by) c) positions velocities]
    let (s, t) = euclid bx by
    print $ (s * bx * 40 + t * by * 99) `mod` (bx * by)
    return ()
