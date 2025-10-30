-- Códigos básicos
answer :: Int
answer = 42

greater :: Bool
greater = answer > 71

-- codigo para yes
{-codigo para yes
-}

yes :: Bool
yes = True

square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

maxi :: Int -> Int -> Int
maxi n m | n >= m   = n
         | otherwise = m

-- Exemplo 1
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 20
vendas 2 = 25
vendas 3 = 28
vendas 4 = 31
vendas n = 0

totalVendas :: Int -> Int
totalVendas n
    | n == 0    = vendas 0
    | otherwise = totalVendas (n-1) + vendas n
    