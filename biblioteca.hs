type BancoDados = [(Pessoa,Livro)]
type Pessoa = String
type Livro = String

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),
               ("Andre","Duna"),
               ("Fernando","Jonathan Strange & Mr. Norrell"),
               ("Fernando","Duna")
              ]

-- livros emprestados

livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((p,l):pls) pessoa | p == pessoa = l : livros pls pessoa
                          | otherwise   = livros pls pessoa

--livros bd pessoa = [livro | (p1, livro) <- bd, p1 == pessoa] -- Forma alternativa

emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos [] _ = []
emprestimos((p,l):pls) livro | l == livro = p : emprestimos pls livro
                             | otherwise = emprestimos pls livro

emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado ((p, l): pls) livro | l == livro = True
                               | otherwise = emprestado pls livro

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos ((p,l):pls) pessoa | p == pessoa = 1 + qtdEmprestimos pls pessoa
                                  | otherwise = qtdEmprestimos pls pessoa

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd pessoa livro = (pessoa,livro) : bd

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] pessoa livro = []
devolver ((p,l):pls) pessoa livro | p == pessoa && l == livro = pls
                                  | otherwise = (p, l) : devolver pls pessoa livro