insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (a:as) | n < a = n : a : as
                | otherwise = a : insert n as
                
ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo n = testaPrimo(n-1)
    where
        testaPrimo 1 = True
        testaPrimo aux | n `mod` aux == 0 = False
                       | otherwise = testaPrimo (aux - 1)

sumPrimeSquares1 :: Int -> Int -> Int
sumPrimeSquares1 x y = foldr (+) 0 (map(\x -> x * x) (filter ehPrimo [x..y]))

sumPrimeSquares2 :: Int -> Int -> Int
sumPrimeSquares2 x y = foldr (+) 0 [z * z | z <- [x..y], ehPrimo z]