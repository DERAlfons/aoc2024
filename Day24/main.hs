import Data.Bits ((.&.), (.|.), xor)
import Data.List (find, partition, intercalate, sort)
import Data.Map (Map, fromList, union, assocs, member, (!))
import Text.Printf (printf)

import My.Parser (parserRegex, run)
import My.Util (explode)

parseOp :: String -> (Int -> Int -> Int)
parseOp "AND" = (.&.)
parseOp "OR"  = (.|.)
parseOp "XOR" = xor

propagate :: Map String Int -> [(String, String, String, String)] -> Map String Int
propagate wires [] = wires
propagate wires gates = let
    (yet, notyet) = partition (\ (in1, in2, _, _) -> in1 `member` wires && in2 `member` wires) gates
    newWires = (\ (in1, in2, op, out) -> (out, parseOp op (wires ! in1) (wires ! in2))) <$> yet in
    propagate (union wires (fromList newWires)) notyet

check :: [(String, String, String, String)] -> Int -> Maybe [(String, String, String, String)]
check gates i = do
    (x, y, op1, ires) <- find (\ (in1, in2, op, _) -> (printf "x%02d" i) `elem` [in1, in2] && (printf "y%02d" i) `elem` [in1, in2] && op == "XOR") gates
    (ires, carry, op2, z) <- find (\ (in1, in2, op, out) -> ires `elem` [in1, in2] && out == (printf "z%02d" i) && op == "XOR") gates
    return [(x, y, op1, ires), (ires, carry, op2, z)]

check2 :: [(String, String, String, String)] -> Int -> [Maybe (String, String, String, String)]
check2 gates i = [
    find (\ (in1, in2, op, _) -> (printf "x%02d" i) `elem` [in1, in2] && (printf "y%02d" i) `elem` [in1, in2] && op == "XOR") gates,
    find (\ (in1, in2, op, out) -> out == (printf "z%02d" i) && op == "XOR") gates]

other :: [(String, String, String, String)] -> Int -> Maybe [(String, String, String, String)]
other gates i = do
    (x, y, op1, ires) <- find (\ (in1, in2, op, _) -> (printf "x%02d" i) `elem` [in1, in2] && (printf "y%02d" i) `elem` [in1, in2] && op == "XOR") gates
    (ires, carry, op2, z) <- find (\ (in1, in2, op, out) -> ires `elem` [in1, in2] && op == "XOR") gates
    return [(x, y, op1, ires), (ires, carry, op2, z)]

sPair :: [(String, String, String, String)] -> Int -> [String]
sPair gates n = [
    printf "z%02d" n,
    (\ (Just [_, (_, _, _, sz)]) -> sz) $ other gates n]

sPairx :: [(String, String, String, String)] -> Int -> [String]
sPairx gates n = let
    [Just (_, _, _, s1), Just (s2, _, _, _)] = check2 gates n in
    [s1, s2]

parserI :: String -> Maybe (String, Int)
parserI = run $ parserRegex "(\\w+): ([01])" $
    \ [w, s] -> (w, read s)

parserC :: String -> Maybe (String, String, String, String)
parserC = run $ parserRegex "(\\w+) (\\w+) (\\w+) -> (\\w+)" $
    \ [in1, op, in2, out] -> (in1, in2, op, out)

main :: IO ()
main = do
    input <- lines <$> readFile "Day24/input.txt"
    let [initialS, connectionsS] = explode "" input
        Just initial = sequence $ parserI <$> initialS
        Just connections = sequence $ parserC <$> connectionsS
        finalState = propagate (fromList initial) connections
    print $ sum $ (\ (_ : e, d) -> d * 2 ^ read e) <$> filter ((== 'z') . head . fst) (assocs finalState)
    sequence $ print <$> check connections <$> [0 .. 44]
    print $ "----"
    sequence $ print <$> check2 connections <$> [10, 21, 33, 39]
    print $ "----"
    sequence $ print <$> other connections <$> [10, 21, 33]
    print $ sPair connections =<< [10, 21, 33]
    print $ sPairx connections 39
    putStrLn $ intercalate "," $ sort $ (sPair connections =<< [10, 21, 33]) ++ sPairx connections 39
    return ()
