-- Primos
ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo n = testaPrimo(n-1)
    where
        testaPrimo 1 = True
        testaPrimo aux | n `mod` aux == 0 = False
                       | otherwise = testaPrimo (aux - 1)

primosEntreSi :: Int -> Int -> Bool
primosEntreSi val_1 val_2 = auxiliar (val_2 - 1)
    where auxiliar 1 = True
          auxiliar x | val_1 `mod` x == 0 && val_2 `mod` x == 0 = False
                     | otherwise = auxiliar (x-1)