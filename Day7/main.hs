import Data.Maybe (maybeToList)
import Data.List (foldl')
import Control.Monad ((<=<))

import My.Parser (parserRegex, parserList, run)

cat :: Integer -> Integer -> Integer
cat n m = n * 10 ^ (length (show m)) + m

valid :: Eq a => [(a -> a -> a)] -> (a, [a]) -> Bool
valid _ (_, []) = False
valid ops (d, (n : ns)) = d `elem` foldl' (\ cs n -> ops <*> cs <*> [n]) [n] ns

parser :: String -> Maybe (Integer, [Integer])
parser = run $ (,) <$>
    parserRegex "(\\d+): " (\ [n] -> read n) <*>
    parserList "" " " "" (
        parserRegex "(\\d+)" (\ [n] -> read n))

main :: IO ()
main = do
    input <- (maybeToList . parser <=< lines) <$> readFile "Day7/input.txt"
    print $ sum $ fst <$> filter (valid [(*), (+)]) input
    print $ sum $ fst <$> filter (valid [(*), (+), cat]) input
    return ()
