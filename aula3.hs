-- Listas

-- Ex: double [1, 2, 3] --> [2, 4, 6]
double :: [Int] -> [Int]
double [] = []
double (x:xs) = (x * 2) : double xs

{- 
Ex2: member [1..10] 9 --> True
Ex2: member [1..10] 99 --> False  
-}
member :: [Int] -> Int -> Bool
member [] a = False
member (x:xs) a | a == x = True 
                | otherwise = member xs a

-- Ex: digits "abcd23xy" -- "23"
digits :: String -> String
digits [] = ""
digits (ch:chs) | ch >= '0' && ch <= '9'= ch : digits chs
                | otherwise = digits chs 

-- Ex: sumPairs [(3,4), (1,1), (1,2)] --> [7, 2, 3]
sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs (pair : pairs) = (fst pair + snd pair) : sumPairs pairs
-- sumPairs ((x,y) : xys) = (x+y) : sumPairs xys