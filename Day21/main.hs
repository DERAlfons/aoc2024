{-# LANGUAGE MultiWayIf #-}

import My.Util (applyN)

type Point = (Int, Int)

panel :: Char -> Point
panel '7' = (0, 0)
panel '8' = (0, 1)
panel '9' = (0, 2)
panel '4' = (1, 0)
panel '5' = (1, 1)
panel '6' = (1, 2)
panel '1' = (2, 0)
panel '2' = (2, 1)
panel '3' = (2, 2)
panel '0' = (3, 1)
panel 'A' = (3, 2)
panel '^' = (3, 1)
panel '<' = (4, 0)
panel 'v' = (4, 1)
panel '>' = (4, 2)

dKeyUD :: Int -> Char
dKeyUD n | n <  0 = '^'
dKeyUD n | n >= 0 = 'v'

dKeyLR :: Int -> Char
dKeyLR n | n <  0 = '<'
dKeyLR n | n >= 0 = '>'

button :: (Char, Char) -> String
button (start, end) = let
    (sy, sx) = panel start
    (ey, ex) = panel end
    (dy, dx) = (ey - sy, ex - sx)
    sqUD = replicate (abs dy) (dKeyUD dy)
    sqLR = replicate (abs dx) (dKeyLR dx) in (if
        | sy == 3 && ex == 0 -> sqUD ++ sqLR 
        | dx < 0 -> sqLR ++ sqUD
        | dy < 0 -> sqUD ++ sqLR
        | otherwise -> sqUD ++ sqLR) ++ "A"

controlSequence :: String -> String
controlSequence a = button =<< zip ('A' : a) a

main :: IO ()
main = do
    input <- lines <$> readFile "Day21/input.txt"
    let codes = applyN 3 controlSequence <$> input
    print $ length <$> codes
    print $ sum $ zipWith (*) (length <$> codes) $ read . take 3 <$> input
    print $ last $ applyN 2 controlSequence <$> ["029A", "980A", "179A", "456A", "379A"]
    return ()
