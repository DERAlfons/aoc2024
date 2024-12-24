import Data.List (transpose, sort, group)
import Data.Function (on)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

sScore :: [Int] -> [Int] -> Int
sScore = loop 0 `on` group . sort
    where
    loop acc [] _ = acc
    loop acc _ [] = acc
    loop acc (nl : nsl) (nr : nsr)
        | head nl == head nr = loop (acc + sum nl * length nr) nsl nsr
        | head nl >  head nr = loop acc (nl : nsl) nsr
        | head nl <  head nr = loop acc nsl (nr : nsr)

main :: IO ()
main = do
    list <- map (map read . words) . lines <$> readFile "Day1/input.txt"
    let [col1, col2] = transpose list
    print $ sum $ zipWith (abs .: subtract) (sort col1) (sort col2)
    print $ sScore col1 col2
    return ()
