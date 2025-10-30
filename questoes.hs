data Tree = No Int Tree Tree | Folha Int deriving Show

teste1 = No 50 (No 25 (No 12 (Folha 6) (Folha 13))
               (No 30 (Folha 26) (Folha 32)))
               (Folha 59)
collapse :: Tree -> [Int]
collapse (Folha n) = [n]
collapse (No val arv1 arv2) = (collapse arv1) ++ val : collapse arv2

lista_ordenada:: [Int] -> Bool
lista_ordenada (a : []) = True
lista_ordenada (a : b :abs) = (a < b) && (lista_ordenada (b:abs))

ordenada :: Tree -> Bool
ordenada t = lista_ordenada (collapse t )

-- Fibonacci
fibonacci_v1 :: [Int]
fibonacci_v1 = gen_fib [0..]

gen_fib :: [Int] -> [Int]
gen_fib a = map fib a

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fibonacci_v2 :: [Int]
fibonacci_v2 = gen_fib1 0 1

gen_fib1 x1 x2 = x1 : gen_fib1 x2 (x1+x2)

primes :: [Int]
primes = gen_primes [2..]

gen_primes :: [Int] -> [Int]
gen_primes (p:ps) = p : gen_primes [x | x <- ps, x `mod`p /= 0]

-- MergeSort

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x: merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

mergeSort_v1 :: [Int] -> [Int]
mergeSort_v1 [] = []
mergeSort_v1 (x:xs) = merge [x] (mergeSort_v1 xs)

mergeSort_v2 :: [Int] -> [Int]
mergeSort_v2 xs = foldr (\ x -> merge [x]) [] xs
