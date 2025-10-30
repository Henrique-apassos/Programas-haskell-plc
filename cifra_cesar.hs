import Data.Char
-- Cifra de Cesar
type Chave = [(Char, Char)]

letras :: [Char]
letras = ['A'..'Z']

cria_chave1 :: Int -> Chave
cria_chave1 n = zip letras ((drop n letras) ++ letras)

cria_chave2 :: Int -> Chave
cria_chave2 n = map cria_par letras
    where cria_par ch = (ch, chr(((ord ch - ord 'A' + n))`mod` 26 + ord 'A'))

crypt :: Chave -> String -> String
crypt chav str = map (troca_letra chav) str


troca_letra [] letra = letra
troca_letra ((ch1, ch2):chs) letra | letra == ch1 = ch2
                                   | otherwise = troca_letra chs letra

