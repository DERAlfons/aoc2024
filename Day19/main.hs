import Data.Map (singleton, fromList, unionWith, findMin, delete)
import Data.Array (Array, listArray, (!), assocs)
import Control.Monad (guard)

import My.Parser (parserRegex, parserList, run)
import My.Util (count, explode, maybeToIO)

firstM :: Functor m => (a -> m c) -> (a, b) -> m (c, b)
firstM f (a, b) = flip (,) b <$> f a

step :: [Array Int Char] -> Array Int Char -> Int -> [Int]
step towels pattern i = do
    t <- towels
    guard $ length t <= length pattern - i
    guard $ and [c == pattern ! (i + j) | (j, c) <- assocs t]
    return $ i + length t

combis :: [Array Int Char] -> Array Int Char -> Int
combis towels pattern = loop (singleton 0 1)
    where
    loop partials
        | null partials = 0
        | i == length pattern = c
        | otherwise = loop $ unionWith (+) (delete i partials) (fromList $ firstM (step towels pattern) (i, c))
        where
        (i, c) = findMin partials

parser :: String -> Maybe [String]
parser = run $ parserList "" ", " "" $
    parserRegex "(\\w+)" head

main :: IO ()
main = do
    input <- lines <$> readFile "Day19/input.txt"
    let [[towelsS], patterns] = explode "" input
    towels <- maybeToIO "bla" $ parser towelsS
    let combiCount = map (combis (map (\ t -> listArray (0, length t - 1) t) towels)) (map (\ p -> listArray (0, length p - 1) p) patterns)
    print $ count (/= 0) combiCount
    print $ sum combiCount
    return ()
