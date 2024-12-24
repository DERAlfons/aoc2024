import Data.Array (Array, listArray, (!), indices, bounds, inRange, range)
import Control.Monad (guard)

countXMAS :: Array (Int, Int) Char -> Int
countXMAS arr = length $ do
    (i, j) <- indices arr
    guard $ arr ! (i, j) == 'X'
    (di, dj) <- [
        (0, -1), (0, 1), (-1, 0), (1, 0),
        (-1, -1), (-1, 1), (1, -1), (1, 1)]
    guard $ inRange (bounds arr) (i + 3 * di, j + 3 * dj)
    guard $ arr ! (i + di, j + dj) == 'M'
    guard $ arr ! (i + 2 * di, j + 2 * dj) == 'A'
    guard $ arr ! (i + 3 * di, j + 3 * dj) == 'S'
    return ()

countX_MAS :: Array (Int, Int) Char -> Int
countX_MAS arr = length $ do
    let ((li, lj), (ui, uj)) = bounds arr
    (i, j) <- range ((li + 1, lj + 1), (ui - 1, uj - 1))
    guard $ arr ! (i, j) == 'A'
    let diag1 = [arr ! (i - 1, j - 1), arr ! (i + 1, j + 1)]
    guard $ diag1 == "MS" || diag1 == "SM"
    let diag2 = [arr ! (i - 1, j + 1), arr ! (i + 1, j - 1)]
    guard $ diag2 == "MS" || diag2 == "SM"
    return ()

main :: IO ()
main = do
    grid <- lines <$> readFile "Day4/input.txt"
    let n = length grid
        m = length $ grid !! 0
        gridArr = listArray ((0, 0), (n - 1, m - 1)) $ concat grid
    print $ countXMAS gridArr
    print $ countX_MAS gridArr
    return ()
