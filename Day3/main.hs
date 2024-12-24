import My.Parser (parserList, parserRegex, run)

parser :: String -> Maybe [Int]
parser = run $ parserList "" "" "" $
    parserRegex "(?:.|\\n)*?mul\\((\\d{1,3}),(\\d{1,3})\\)" $
        \ [d1, d2] -> read d1 * read d2

parseBlocks :: String -> Maybe [String]
parseBlocks = run $ parserList "" "(.|\\n)*?do\\(\\)" "" $
    parserRegex "((?:.|\\n)*?)(?:don't\\(\\)|$)" head

main :: IO ()
main = do
    input <- readFile "Day3/input.txt"
    let Just products = parser input
    print $ sum products
    let Just blocks = parseBlocks input
        Just blockProducts = traverse parser blocks
    print $ sum $ concat blockProducts
    return ()
