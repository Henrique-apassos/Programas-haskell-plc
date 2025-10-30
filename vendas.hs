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

total :: (Int -> Int ) -> Int -> Int
total f 0 = f 0
total f n = total f (n-1) + f n

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = max (maxVendas (n-1)) (vendas n)

vendasIguaisA :: Int -> Int -> Int
-- Casos base
vendasIguaisA valor 0 | vendas 0 ==  valor = 1
                      | otherwise = 0
-- caso recursivo
vendasIguaisA valor sem | vendas sem == valor = 1 + vendasIguaisA valor (sem - 1)
                        | otherwise = vendasIguaisA valor (sem - 1)

addEspacos :: Int -> String
addEspacos 0 = " "
addEspacos n = " " ++ addEspacos(n-1)

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr( cabecalho
                             ++ imprimeSemanas n
                             ++ imprimeTotal n
                             ++ imprimeMedia n)

imprimeSemanas :: Int -> String
imprimeSemanas 0 = "0" ++ paraDireita 7 (show (vendas 0)) ++ "\n"
imprimeSemanas n = imprimeSemanas (n-1) ++ show n ++ paraDireita 7 (show (vendas n)) ++ "\n"

paraDireita :: Int -> String -> String
paraDireita 0 st = st
paraDireita n st = addEspacos n ++ st

cabecalho :: String
cabecalho = "Semana     Vendas\n"

imprimeTotal :: Int -> String
imprimeTotal n = "Total    " ++ show (totalVendas n) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "Média    " ++ show (fromIntegral (totalVendas n) / fromIntegral (n + 1)) ++ "\n"
