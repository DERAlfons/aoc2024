import Data.List (tails, partition)
import Data.Set (Set, singleton, union, member, notMember)
import Data.Map (Map, fromListWith, (!))
import My.Util (explode)

check :: Ord a => Map a (Set a) -> [a] -> Bool
check prvs pages = all checkFirst $ init $ tails pages
    where
    checkFirst (p : nxts) = all (`notMember` (prvs ! p)) nxts

correct :: Ord a => Map a (Set a) -> [a] -> [a]
correct _ [] = []
correct prvs (p : ps) = let
    (right, left) = break (`member` (prvs ! p)) $ reverse ps in
    if left == []
        then p : correct prvs ps
        else correct prvs $ reverse left ++ [p] ++ reverse right

middle :: [a] -> a
middle a = a !! (length a `div` 2)

main :: IO ()
main = do
    input <- lines <$> readFile "Day5/input.txt"
    let [rulesS, seriesS] = explode "" input
        rules = (\ [p1, p2] -> (p2, singleton p1)) . explode '|' <$> rulesS
        series = explode ',' <$> seriesS
        prvs = fromListWith union rules
        (valid, invalid) = partition (check prvs) series
    print $ sum $ read . middle <$> valid
    print $ sum $ read . middle . correct prvs <$> invalid
    return ()
