getPartition :: [Char] -> [(Int, Int, Int)]
getPartition = loop 0
    where
    loop _ [] = []
    loop i [d] = [(i, read [d], 0)]
    loop i (df : de : ds) = (i, read [df], read [de]) : loop (i + 1) ds

defrag :: [(Int, Int, Int)] -> [(Int, Int, Int)]
defrag = loop <*> reverse
    where
    loop ((i1, s1, e1) : fs) ((i2, s2, _) : fsb)
        | i1 >  i2 = []
        | i1 == i2 = [(i2, s2, 0)]
        | s2 == e1 = (i1, s1, 0) : (i2, s2, 0) : loop fs fsb
        | s2 <  e1 = (i1, s1, 0) : (i2, s2, 0) : loop ((i1, 0, e1 - s2) : fs) fsb
        | s2 >  e1 = (i1, s1, 0) : (i2, e1, 0) : loop fs ((i2, s2 - e1, 0) : fsb)

defragBlock :: [(Int, Int, Int)] -> [(Int, Int, Int)]
defragBlock = loop [] <*> reverse
    where
    loop acc fs [] = reverse $ cleanup (reverse fs) (reverse acc)
    loop acc fs (f : rest) = case insert fs f of
        Just a -> loop (f : acc) a rest
        Nothing -> loop acc fs rest

    cleanup fs [] = fs
    cleanup (f @ (i1, s1, e1) : fs @ ((i3, s3, e3) : fss)) (a @ (i2, _, _) : as)
        | i1 == i2 = cleanup ((i3, s3, e3 + s1 + e1) : fss) as
        | i1 /= i2 = f : cleanup fs (a : as)

insert :: [(Int, Int, Int)] -> (Int, Int, Int) -> Maybe [(Int, Int, Int)]
insert ((i1, s1, e1) : rest) (i2, s2, _)
    | i1 == i2 = Nothing
    | s2 >  e1 = ((i1, s1, e1) :) <$> insert rest (i2, s2, 0)
    | s2 <= e1 = Just $ (i1, s1, 0) : (i2, s2, e1 - s2) : rest

checksum :: [(Int, Int, Int)] -> Int
checksum = loop 0
    where
    loop _ [] = 0
    loop n ((_, 0, e) : rest) = loop (n + e) rest
    loop n ((i, c, e) : rest) = n * i + loop (n + 1) ((i, c - 1, e) : rest)

main :: IO ()
main = do
    [input] <- lines <$> readFile "Day9/input.txt"
    print $ checksum $ defrag $ getPartition input
    print $ checksum $ defragBlock $ getPartition input
    return ()
