-- Criação de novos tipos de dados
data Estacao = Verao | Inverno | Outono | Primavera
    deriving Show

data Temp = Quente | Frio
    deriving Show

clima :: Estacao -> Temp
clima Inverno = Frio
clima _ = Quente

type Nome = String -- É apenas um apelido, não cria um novo tipo
type Idade = Int
data Pessoas = Pessoa Nome Idade

jose :: Pessoas
jose = Pessoa "Jose" 25

showPerson :: Pessoas -> String
showPerson (Pessoa n a) = n ++ " -- "++ show a

-- Exercicio
data Shape = Circle Float | Rectangle Float Float | Square Float

area :: Shape -> Float
area (Circle r) = pi*(r^2)
area (Rectangle a b) = a * b
area (Square a) = a^2

-- Tipos recursivos
data Expr = Lit Int       | -- Representa uma árvore
            Add Expr Expr |
            Sub Expr Expr |
            Mult Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)

-- Exercício 2
showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(Add " ++ show (eval e1) ++ " + " ++ show (eval e2) ++ " )"
showExpr (Sub e1 e2) = "(Sub " ++ show (eval e1) ++ " - " ++ show (eval e2) ++ " )"
showExpr (Mult e1 e2) = "(Mult " ++ show (eval e1) ++ " * " ++ show (eval e2) ++ " )"

data List t = Nil | Const t (List t) deriving Show

toList :: List t -> [t]
toList Nil = []
toList (Const a (Nil)) = [a]
toList (Const a (as)) = a : toList as

fromList :: [t] -> List t
fromList [] = Nil
fromList (b:bs) = (Const b(fromList bs))

data Tree t = NilT |
              Node t (Tree t) (Tree t)
              deriving Show 

teste1, teste2, teste3 :: Tree Int
teste1 = Node 4 NilT NilT
teste2 = Node 27 teste1 NilT
teste3 = Node 91 NilT teste2

depth :: Tree t -> Int
depth NilT = 0
depth (Node value NilT NilT) = 1
depth (Node value arvore_1 arvore_2) = 1 + max (depth arvore_1) (depth arvore_2)

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node value arv1 arv2) = value : collapse arv1 ++ collapse arv2

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree funcao NilT = NilT
mapTree funcao (Node val a1 a2) = Node (funcao val) (mapTree funcao a1) (mapTree funcao a2)