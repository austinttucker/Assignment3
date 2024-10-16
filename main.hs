main :: IO()
main = print (counting_tuples "ac/dc")
counting_tuples :: (Eq a) => [a] -> [(a, Int)]
counting_tuples list = map (\x -> (x, length (filter (== x) list))) list
descending :: (Ord a) => [a] -> Bool
descending list
    | null list           = True
    | length list == 1    = True
    | head list > list!!1 = descending (tail list)
    | otherwise           = False
divs :: Int -> [Int]
divs num
    | num <= 0  = []
    | otherwise = [i | i <- [1..num], num `mod` i == 0]
isPrime :: Int -> Bool
isPrime num
    | num <= 1   = False
    | otherwise  = null [i | i <- [2..num-1], num `mod` i == 0]
divisibleBy23579 :: Int -> Bool
divisibleBy23579 num
    | even num = False
    | num `mod` 3 == 0 = False
    | num `mod` 5 == 0 = False
    | num `mod` 7 == 0 = False
    | num `mod` 9 == 0 = False
    | otherwise        = True
fakePrimes :: [Int]
fakePrimes = [i | i <- [2..], divisibleBy23579 i, not (isPrime i)]
operate :: [Int] -> [Num] -> [Int]
operate list