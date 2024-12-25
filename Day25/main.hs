import Data.List (transpose, partition)
import Control.Monad (guard)

import My.Parser (parserRegex, parserList, run)
import My.Util (count)

combis :: [[[Char]]] -> [[[Char]]] -> Int
combis locks keys = length $ do
    lock <- locks
    key <- keys
    guard $ and $ zipWith (<=) (count (== '#') <$> lock) (count (== '.') <$> key)
    return ()

parser :: String -> Maybe ([[[Char]]], [[[Char]]])
parser = run $ partition ((== '#') . (!! 0) . (!! 0)) . map transpose <$>
    parserList "" "\\n\\n" "" (
        parserList "" "\\n" "" $
            parserRegex "([.#]{5})" head)

main :: IO ()
main = do
    Just (locks, keys) <- parser <$> readFile "Day25/input.txt"
    print $ combis locks keys
    return ()
