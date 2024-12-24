import Control.Applicative (liftA2)
import My.Util (count)

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <.> f = fmap g . f

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

infixr 9 <.>
infixl 4 <||>

pairwise :: (a -> a -> Bool) -> [a] -> Bool
pairwise f = and . (zipWith f <*> drop 1)

dampened :: (a -> a -> Bool) -> [a] -> Bool
dampened f = pairwise f . drop 1 <||> loop
    where
    loop (n1 : n2 : ns)
        | f n1 n2 = loop (n2 : ns)
        | otherwise = (pairwise f) (n1 : ns)

safeInc :: Int -> Int -> Bool
safeInc a b = b > a && b <= a + 3

safeDec :: Int -> Int -> Bool
safeDec a b = b < a && b >= a - 3

safe :: [Int] -> Bool
safe = pairwise safeInc <||> pairwise safeDec

safeDampened :: [Int] -> Bool
safeDampened = dampened safeInc <||> dampened safeDec

main :: IO ()
main = do
    reports <- (read <.> words) <.> lines <$> readFile "Day2/input.txt"
    print $ count safe reports
    print $ count safeDampened reports
    return ()
