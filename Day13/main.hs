import Data.Ratio ((%), numerator, denominator)

import My.Util (maybeToIO)
import My.Parser (parserRegex, parserList, run)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

cost :: ((Int, Int), (Int, Int), (Int, Int)) -> [Int]
cost ((a, c), (b, d), (x, y)) = let
    det = a * d - b * c
    [pa, pb] = (% det) <$> [d * x - b * y, -c * x + a * y] in
    [3 * numerator pa + numerator pb | all ((== 1) . denominator) [pa, pb]]

parser :: String -> Maybe [((Int, Int), (Int, Int), (Int, Int))]
parser = run $ parserList "" "\\n\\n" "" $ (,,) <$>
    parserRegex "Button A: X\\+(\\d+), Y\\+(\\d+)\\n"
        (\ [n1, n2] -> (read n1, read n2)) <*>
    parserRegex "Button B: X\\+(\\d+), Y\\+(\\d+)\\n"
        (\ [n1, n2] -> (read n1, read n2)) <*>
    parserRegex "Prize: X=(\\d+), Y=(\\d+)"
        (\ [n1, n2] -> (read n1, read n2))

main :: IO ()
main = do
    input <- maybeToIO "bla" . parser =<< readFile "Day13/input.txt"
    print $ sum $ cost =<< input
    print $ sum $ cost =<< fmap (both (+ 10000000000000)) <$> input
    return ()
