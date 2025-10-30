somaDez :: Int -> Int
somaDez x = x + 10

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f(f x)

-- applyTwice somaDez 50 --> somaDez (somaDez 50) --> somaDez (60) --> 70

-- Exercício
isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n | f n >= f (n-1) = isCrescent f (n-1)
               | otherwise = False
-- Alternativa: 
-- isCrescent f n = ( f n >= f (n-1)) && (isCrescent f (n-1))

sequencia :: Int -> Int
sequencia 0 = 1
sequencia 1 = 1
sequencia 2 = 2
sequencia 3 = 2
sequencia 4 = 3
sequencia 5 = 2
sequencia n = 0
