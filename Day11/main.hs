import Data.Map (Map, fromList, fromListWith, assocs)

import My.Util (explode, applyN)

step :: Int -> [Int]
step 0 = [1]
step n
    | even (length s) = let
        (left, right) = splitAt (length s `div` 2) s in
        [read left, read right]
    | otherwise = [n * 2024]
    where
    s = show n

update :: Map Int Int -> Map Int Int
update stones = fromListWith (+) $ do
    (n, c) <- assocs stones
    zip (step n) [c, c ..]

main :: IO ()
main = do
    [input] <- lines <$> readFile "Day11/input.txt"
    let stones = read <$> explode ' ' input
        stoneMap = fromList $ zip stones [1, 1 ..]
    print $ sum $ applyN 25 update stoneMap
    print $ sum $ applyN 75 update stoneMap
    return ()
