import Data.List (zip4, foldl1')
import Data.Map (Map, fromListWith, unionWith)
import Data.Bits (xor)

import My.Util (applyN)

type DifSeq = (Int, Int, Int, Int)

next :: Int -> Int
next n = let
    m = 16777216
    r1 = (n `xor` (n * 64)) `mod` m
    r2 = (r1 `xor` (r1 `div` 32)) `mod` m in
    (r2 `xor` (r2 * 2048)) `mod` m

differences :: [Int] -> [Int]
differences ns = zipWith (-) (drop 1 ns) ns

priceMap :: Int -> Map DifSeq Int
priceMap n = let
    secrets = take 2000 $ iterate next n
    prices = (`mod` 10) <$> secrets
    ds = differences prices in
    fromListWith (flip const) $ zip (zip4 <*> drop 1 <*> drop 2 <*> drop 3 $ ds) (drop 4 prices)
    
main :: IO ()
main = do
    input <- (read <$>) . lines <$> readFile "Day22/input.txt"
    print $ sum $ applyN 2000 next <$> input
    print $ maximum $ foldl1' (unionWith (+)) $ priceMap <$> input
    return ()
