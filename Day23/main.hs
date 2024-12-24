import Data.List (sort, intercalate)
import Data.Map (Map, fromList, size, fromListWith, assocs, member, (!), (!?), mapKeys, keys, findWithDefault)
import Control.Monad (guard)

import My.Parser (parserRegex, parserList, run)

intersectAsc :: Ord a => [a] -> [a] -> [a]
intersectAsc [] _ = []
intersectAsc _ [] = []
intersectAsc (a : as) (b : bs)
    | a <  b = intersectAsc as (b : bs)
    | a >  b = intersectAsc (a : as) bs
    | a == b = a : intersectAsc as bs

conn3 :: Map String [String] -> Int
conn3 conns = length $ do
    (c, ps) <- assocs conns
    p1 <- ps
    guard $ p1 `member` conns
    p2 <- filter (> p1) $ ps
    guard $ p2 `elem` (conns ! p1)
    guard $ c !! 0 == 't' || p1 !! 0 == 't' || p2 !! 0 == 't'
    return ()

cluster :: Map String [String] -> Map [String] [String] -> Map [String] [String]
cluster conns pools = fromList $ do
    (cs, ps) <- assocs pools
    pNew <- ps
    return (pNew : cs, intersectAsc ps (findWithDefault [] pNew conns))

maxCluster :: Map String [String] -> [String]
maxCluster conns = loop $ mapKeys return conns
    where
    loop pools
        | size pools == 1 = keys pools !! 0
        | otherwise = loop $ cluster conns pools

parser :: String -> Maybe (Map String [String])
parser = run $ fromListWith (++) <$> parserList "" "\\n" "" (
    parserRegex "(\\w+)-(\\w+)" $
        \ [c1, c2] -> if c1 < c2 then (c1, [c2]) else (c2, [c1]))

main :: IO ()
main = do
    Just inputU <- parser <$> readFile "Day23/input.txt"
    let input = sort <$> inputU
    print $ conn3 input
    putStrLn $ intercalate "," $ reverse $ maxCluster input
    return ()
