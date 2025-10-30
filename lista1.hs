-- A. Detecção de réplica
isReplica :: String -> Int -> Char -> Bool
isReplica "" 0 _ = True
isReplica "" _ _ = False
isReplica _ 0 _ = False
isReplica (a:as) n c | a == c && isReplica as (n-1) c = True
                     | otherwise = False

-- B. Multiplicação de listas
mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 [] (b:bs) = 0 : mul2 [] bs
mul2 (a:as) [] = 0 : mul2 as []
mul2 (a:as) (b:bs) = a * b : mul2 as bs

-- C. Estatísticas da fatura do cartão
minMaxCartao :: String -> (Double, Double)
minMaxCartao "" = (0.0, 0.0)
minMaxCartao str = (head lista, last lista)
  where
    lista = qSort (strDub (split ';' str))

split :: Char -> String -> [String]
split c = foldr step [""]
  where
    step char (x:xs) | char == c = "":x:xs
                     | otherwise = (char:x):xs

ehDub :: String -> Bool
ehDub "" = False
ehDub s = testaDub s 0 False
  where 
    testaDub [] countPontos temDigito = countPontos == 1 && temDigito
    testaDub (a:as) contPontos temDigito | a >= '0' && a <= '9' = testaDub as contPontos True
                                         | a == '.' = testaDub as (contPontos + 1) temDigito
                                         |otherwise = False

strDub :: [String] -> [Double]
strDub [] = []
strDub (st : sts) | ehDub st = read st : strDub sts
                  | otherwise = strDub sts

qSort :: [Double] -> [Double]
qSort [] = []
qSort (x : xs) = qSort [y | y <- xs, y < x] ++ [x] ++ qSort [y | y <- xs, y >= x]

-- D. Fatura do cartão
logMes :: String -> String -> Double
logMes "" "" = 0.0
logMes ref str = foldl (+) 0.0 listaMes
  where
    listaMes = strDub (split ';' listaFilt)
      where
        listaFilt = foldr (++) [] (filtraLista ref (split ' ' str))

filtraLista :: String -> [String] -> [String]
filtraLista _ [] = []
filtraLista ref (m:ms) | comecaCom ref m = m : filtraLista ref ms 
                       | otherwise = filtraLista ref ms

comecaCom :: String -> String -> Bool
comecaCom [] _ = True
comecaCom _ [] = False
comecaCom (x : xs) (y : ys) | x == y = comecaCom xs ys
                            | otherwise = False

-- E. Some até... 
sumTo :: Int -> Int
sumTo 0 = 0
sumTo n = n + sumTo (n-1)

-- F. Lista de novos alunos
bSortPass :: [String] -> [String]
bSortPass [] = []
bSortPass [x] = [x]
bSortPass (x:y:xs) | x <= y    = x : bSortPass (y:xs)
                   | otherwise = y : bSortPass (x:xs)

bSort :: [String] -> [String]
bSort str | str == pass = str
          | otherwise  = bSort pass
          where
            pass = bSortPass str

-- G. Binário para inteiro
btoi :: String -> Int
btoi "" = 0
btoi (x:xs) | x == '1' = 2^length xs + btoi xs
            | otherwise = btoi xs