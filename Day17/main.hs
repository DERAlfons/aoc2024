import Data.Bits (xor)
import Data.Array (Array, listArray, bounds, (!))

import My.Parser (parserRegex, parserList, run)
import My.Util (maybeToIO)

data Expr = Const Int | Var String | Op String Expr Expr

instance Show Expr where
    show (Const c) = show c
    show (Var v) = v
    show (Op op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"

runP :: Int -> Int -> Int -> Int -> Array Int Int -> [Int]
runP a b c ip p
    | ip > snd (bounds p) - 1 = []
    | otherwise = case p ! ip of
        0 -> runP (a `div` 2 ^ co) b c (ip + 2) p
        1 -> runP a (b `xor` v) c (ip + 2) p
        2 -> runP a (co `mod` 8) c (ip + 2) p
        3 | a == 0 -> runP a b c (ip + 2) p
        3 | a /= 0 -> runP a b c v p
        4 -> runP a (b `xor` c) c (ip + 2) p
        5 -> (co `mod` 8) : runP a b c (ip + 2) p
        6 -> runP a (a `div` 2 ^ co) c (ip + 2) p
        7 -> runP a b (a `div` 2 ^ co) (ip + 2) p
    where
    v = p ! (ip + 1)
    co = case v of
        4 -> a
        5 -> b
        6 -> c
        otherwise -> v

runS :: Array Int Int -> Expr -> Expr -> Expr -> Int -> ([Expr], Expr, Expr, Expr)
runS p = loop []
    where
    loop out a b c ip
        | ip > snd (bounds p) - 1 = (out, a, b, c)
        | otherwise = case p ! ip of
            0 -> loop out (Op ">>" a co) b c (ip + 2)
            1 -> loop out a (Op "^" b (Const v)) c (ip + 2)
            2 -> loop out a (Op "%" co (Const 8)) c (ip + 2)
            3 -> (out, a, b, c)
            4 -> loop out a (Op "^" b c) c (ip + 2)
            5 -> loop ((Op "%" co (Const 8)) : out) a b c (ip + 2)
            6 -> loop out a (Op ">>" a co) c (ip + 2)
            7 -> loop out a b (Op ">>" a co) (ip + 2)
        where
        v = p ! (ip + 1)
        co = case v of
            4 -> a
            5 -> b
            6 -> c
            otherwise -> Const v

findA :: Array Int Int -> [Int] -> Int
findA _ [] = 0
findA p out = let
    a = findA p (tail out) in
    loop (a * 8)
    where
    loop a
        | runP a 0 0 0 p == out = a
        | otherwise = loop (a + 1)

parser :: String -> Maybe (Int, Int, Int, [Int])
parser = run $ (,,,) <$>
    parserRegex "Register A: (\\d+)\\n" (\ [r] -> read r) <*>
    parserRegex "Register B: (\\d+)\\n" (\ [r] -> read r) <*>
    parserRegex "Register C: (\\d+)\\n\\n" (\ [r] -> read r) <*>
    parserList "Program: " "," "" (
        parserRegex "(\\d)" (\ [s] -> read s))

main :: IO ()
main = do
    (a, b, c, stackL) <- maybeToIO "bla" . parser =<< readFile "Day17/input.txt"
    let stack = listArray (0, length stackL - 1) stackL
    print $ runP a b c 0 stack
    print $ runS stack (Var "a") (Var "b") (Var "c") 0
    print $ findA stack stackL
    return ()
