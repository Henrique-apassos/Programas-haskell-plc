-- A. Safe second
import Prelude hiding (Maybe (..))
data Maybe a = Just a |
               Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond [x] = Nothing
safeSecond (x : y : xys) = Just y

-- B. Robô 1
data Command = Forward Int | Backward Int | TurnLeft | TurnRight
               deriving (Eq, Show, Read)

destination :: (Int, Int) -> [Command] -> (Int, Int)
destination (a0,a1) [] = (a0, a1)
destination start comandos = anda start 0 comandos
    where
        anda :: (Int, Int) -> Int -> [Command] -> (Int, Int)
        anda pos _ [] = pos
        anda pos dir (cmd:cmds) = case cmd of
            TurnLeft -> anda pos ((dir - 1)`mod` 4) cmds
            TurnRight -> anda pos ((dir + 1) `mod` 4) cmds
            Forward n -> anda (move pos dir n) dir cmds
            Backward n -> anda (move pos ((dir + 2) `mod` 4) n) dir cmds

move :: (Int,Int) -> Int -> Int -> (Int,Int)
move (a0,a1) dir n = case dir of
    0 -> (a0, a1+n) -- Norte
    1 -> (a0+n,a1) -- Leste
    2 -> (a0,a1-n) -- Sul
    3 -> (a0-n,a1) -- Oeste

-- C. Robô 2
data Direction = North | South | West | East
                 deriving (Read, Show)

faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces dir (x:xs) = faces (posicao (dirtoInt dir) (vira x)) xs

inttoDir :: Int -> Direction
inttoDir 0 = North
inttoDir 1 = East
inttoDir 2 = South
inttoDir 3 = West

dirtoInt :: Direction -> Int
dirtoInt North = 0
dirtoInt East = 1
dirtoInt South = 2
dirtoInt West = 3

vira :: Command -> Int
vira (Forward _) = 0
vira (Backward _) = 0
vira TurnLeft = -1
vira TurnRight = 1

posicao :: Int -> Int -> Direction
posicao n 0 = inttoDir n
posicao a b | (a + b) >= 0 = inttoDir ((a + b) `mod` 4)
            | otherwise = inttoDir ((4 + a + b) `mod` 4)


-- D. Altura de árvore 
data Tree t = Node t (Tree t) (Tree t)
              | Nilt
              deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node n Nilt Nilt) = 1
alturaArvore (Node n ar1 ar2) = 1 + max (alturaArvore ar1) (alturaArvore ar2)

-- E. Editor de Texto
data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

editText :: String -> [Cmd] -> String
editText txt [] = txt
editText txt cmd = editTextC ("", txt) cmd

insert :: String -> String -> String
insert txt p = p ++ txt


delete :: String -> Int -> String
delete "" _ = ""
delete (a:as) n | n > 1 = delete as (n-1)
                | otherwise = as

backspace :: String -> Int -> String
backspace "" _ = ""
backspace txt n = take (length txt - n) txt

editTextC :: (String, String) -> [Cmd] -> String
editTextC ("", "")  _ = ""
editTextC (txt1, txt2)  [] = txt1 ++ txt2
editTextC (txt1, txt2) (cm: cms) = case cm of
    Insert s -> editTextC (txt1, (insert txt2 s)) cms
    Delete n -> editTextC (txt1, (delete txt2 n)) cms
    Backspace n -> editTextC (backspace txt1 n, txt2) cms
    Cursor n -> cursorC (txt1, txt2) n cms

parteStr :: String -> Int -> (String, String)
parteStr "" _ = ("","")
parteStr x 0 = ("", x)
parteStr (x:xs) n = (x:first, rest)
    where
        (first, rest) = parteStr xs (n-1)

cursorC :: (String, String) -> Int -> [Cmd] -> String
cursorC ("", "") _ _ = ""
cursorC (txt1, txt2) n com
    | n > 0 = editTextC (txt1 ++ take n txt2, drop n txt2) com
    | n < 0 = editTextC (take (length txt1 + n) txt1, drop (length txt1 + n) txt1 ++ txt2) com
    | otherwise = editTextC (txt1, txt2) com

-- F. Jeff Manga quer estudar na Canergie Melon!
contagemNotas :: [String] -> [String] -> Int
contagemNotas [] corte = 0
contagemNotas notas [] = 0
contagemNotas notas (c:cs) | media notas >= alfToNota c = 1 + contagemNotas notas cs
                           | otherwise = contagemNotas notas cs


alfToNota :: String -> Float
alfToNota "" = 0.0
alfToNota (n:ns) | n == '+' = 0.4
                 | n == '-' = -0.3
                 | n == 'A' = 9.3 + alfToNota ns
                 | n == 'B' = 8.3 + alfToNota ns
                 | n == 'C' = 7.3 + alfToNota ns
                 | n == 'D' = 6.3 + alfToNota ns
                 | n == 'F' = 5.9
                 | otherwise = 0

media :: [String] -> Float
media [] = 0.0
media notas = foldl (+) 0.0 (map alfToNota notas) / tamLStr notas

tamLStr :: [String] -> Float
tamLStr [] = 0.0
tamLStr (a:as) = 1.0 + tamLStr as

-- G. Maior Diametro
maiorDiametro :: Ord t => Tree t -> Int
maiorDiametro Nilt = 0
maiorDiametro (Node n esq dir) = maximum [maiorDiametro esq, maiorDiametro dir, alturaArvore esq + alturaArvore dir + 1]
