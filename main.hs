import Data.Char (isDigit)

calcS :: Float->Float->Float->Float
calcS a b c = (a + b + c)/2

a a b c = sqrt (s * (s-a) * (s-b) * (s-c))
          where s = calcS a b c


anoBissexto :: Integer -> Bool
anoBissexto ano | (ano `mod` 400 == 0) = True 
                | (ano `mod` 4 == 0 ) && (ano `mod` 100 /= 0) = True
                | otherwise = False

dataValida :: Integer->Integer->Integer->IO ()
dataValida dia mes ano  | (dia >= 1) && (dia <= 31) && (mes >= 1) && (mes <= 12) && (ano >= 1) =   putStrLn ("Data válida")
                        | otherwise = putStrLn ("Data inválida")

par :: Integer->Bool
par num = if num `mod` 2 == 0 then True else False

conceito :: Float->Char

conceito nota | (nota < 4) = 'E'
              | (nota >= 4) && (nota <= 5.99) = 'D'  
              | (nota >= 6) && (nota <= 7.49) = 'C'  
              | (nota >= 7.5) && (nota <= 8.99) = 'B'  
              | (nota >= 9) = 'A'  

type Nome = String
type Idade = Int
type Peso = Float
type Esporte = String

type Pessoa = (Nome,Idade,Peso,Esporte)

bancoDeDados:: Int -> Pessoa
bancoDeDados id | id == 1 = ("Jorgin",18,75,"Futebol")
                | id == 2 = ("Mafalda",28,75,"Halterofilismo")
                | id == 3 = ("cuzin",12,55,"Natação")
                | id == 4 = ("peludin",33,95,"Powerlifting")
                | otherwise = ("nil",0,0,"nil")

getNome:: Int -> String
getNome id = case bancoDeDados id of (name,_,_,_) -> name 

pessoaMaisnova:: Int->Int->Pessoa
pessoaMaisnova p1 p2 = if idade1 < idade2 
                        then 
                            (bancoDeDados p1) 
                        else 
                            (bancoDeDados p2)
                       where (nome1,idade1,_,_) = bancoDeDados p1
                             (nome2,idade2,_,_) = bancoDeDados p2

potencia:: Int -> Int -> Int
potencia n 0 = 1
potencia n p = n * (potencia n (p - 1)) 

parRec:: Int->Bool
parRec 0 = True
parRec n = not (parRec(n-1))

somatorio:: Int -> Int 
somatorio 0 = 0
somatorio n = n + (somatorio (n-1))


pessoasSelecionadas 1 = 1
pessoasSelecionadas idlim = 1 + pessoasSelecionadas(idlim - 1) 

-- menorIdadeBase:: Int->Pessoa
-- menorIdadeBase 1 = bancoDeDados 1
-- menorIdadeBase idlim = pessoaMaisnova idlim (menorIdadeBase(idlim-1)) 

--somaDigitos 1

--listas e recursão

letrasAlfabeto = ['a'..'z']

listaDecrescente = [200,199..0]

inversoLista [] = []
inversoLista (x:xs) = (inversoLista xs) ++ [x] 

pegarNPrimeiros 0 _ = []
pegarNPrimeiros _ [] = []
pegarNPrimeiros n (x:xs) = [x] ++ pegarNPrimeiros (n-1) xs

pegarNUltimos 0 _ = []
pegarNUltimos _ [] = []
pegarNUltimos n xs =  inversoLista(pegarNPrimeiros n (inversoLista xs))

removerNPrimeiros 0 l = l 
removerNPrimeiros _ [] = [] 
removerNPrimeiros n (_:xs) = removerNPrimeiros(n-1) xs 

removerUltimoElemento [] = []
removerUltimoElemento (_:[]) = [] 
removerUltimoElemento (x:xs) = x : removerUltimoElemento xs 
        
removerNUltimos _ [] = []
removerNUltimos 0 xs = xs
removerNUltimos n xs = inversoLista (removerNPrimeiros n (inversoLista xs))

removerNEsimoElemento:: Int -> [a] -> [a]
removerNEsimoElemento n xs = pegarNPrimeiros (n-1) xs ++ pegarNUltimos (n+1) xs    

-- listas geradores
operacaoLista:: [Int] -> Int -> [Int]
operacaoLista l op | op == 1 = [x | x <- l, x `mod` 2 == 0]
                   | op == 2 = [x | x <- l, x `mod` 2 /= 0]
                   | otherwise = l

somaTuplas:: [(Int,Int)] -> [Int]
somaTuplas l = [x+x' | (x,x') <- l]

listaPares:: [Int] -> [Int] -> [(Int,Int)]
listaPares xs ys = [(x,y) | x <- xs, y <- ys] 

removerChar:: Char -> String -> String
removerChar c str = [s | s <- str, s /= c]

type Codigo = String
type Nota = Float
type NomeA = String
type Aluno = (Codigo,Nota,NomeA)

-- baseDeDadosAlunos:: Int -> Aluno
-- baseDeDadosAlunos n | n == 1 = ("1224-5",18.5,"José")
                    -- | n == 1 = ("1226-5",12.5,"Alfredo")  
                    -- | n == 1 = ("1264-5",11.5,"Gabriela")
                    -- | n == 1 = ("1124-2",17.5,"Joaquim")
                    -- | otherwise = ("nil",0,"nil")

baseDeDadosAlunos :: [Aluno]
baseDeDadosAlunos = [("1224-5",18.5,"José"),("1226-5",6.5,"Alfredo"),("1264-5",1.5,"Gabriela"),("1124-2",17.5,"Joaquim")]
notasAcimaDeOito :: [Codigo]
notasAcimaDeOito = [cod | (cod,nota,_) <- baseDeDadosAlunos, nota > 8]

nomes = ["Jose","Amendoim","Dunim","Pedro","Palindromo","Pao de batata"]

comecaCom :: Char -> String -> Bool
comecaCom letra (x:xs) | x == letra = True
                       | otherwise = False

nomesComN :: Char -> [String] -> [String]
nomesComN c nomes = [nome | nome <- nomes, comecaCom c nome] 

--funções de alta ordem

-- app :: (a->b) -> (a,a) -> (b,b)
-- app f (x,y) = (f x, f y)
-- > app chr (65, 70)
-- ('A','F’)
-- > app tan (65, 70)
-- (-1.4700382576,1.22195991813)

tamanhoStrings:: [String] -> (String -> Int) -> [(String,Int)]
tamanhoStrings [] _ = [("",0)]
tamanhoStrings strings f = [(x, f x) | x <- strings]

tamString :: String -> Int
tamString [] = 0
tamString (h:hs) = tamString hs + 1

--Generalizando filtro de uma lista
takeWhilem :: (a -> Bool) -> [a] -> [a]
takeWhilem _ [] = []
takeWhilem f (p:ps) | (f p) = p: takeWhilem f ps
                   | otherwise = takeWhilem f ps

-- esse filtro que tem que passar como (menorQue 6), por exemplo
menorQue :: Float -> Float -> Bool
menorQue n x | x < n = True
             | otherwise = False

-- Expressao lambda
sucessor x = (\x -> x + 1) x
dobro = \x -> x+x
cauda = \(_:c) -> c

-- Pegando a media dos alunos com função lambda
-- listaAlunos :: Curso
-- listaAlunos = [(1234, "Jose Azevedo", 13.2), (2345, "Carlos Silva", 9.7),(3456,
-- "Rosa Mota", 17.9)]
-- mediaDasNotas :: Curso->Float
-- mediaDasNotas lista = (/) (sum (map (\(_,_,n)->n) lista)) (fromIntegral (length
-- lista))

-- aplica uma certa funcao duas vezes
duasVezes :: (a -> a) -> a -> a
duasVezes f x = f (f x)

-- itera uma lista e aplica a funcao de aplicar duas vezes na lista
aplicarDuasVezes :: (a -> a) -> [a] -> [a]
aplicarDuasVezes f xs = map (duasVezes f) xs

-- chama a funcao que aplica duas vezes alguma funcao na lista
resultadoAltaOrdem :: [Int] -> [Int]
resultadoAltaOrdem lista = aplicarDuasVezes sucessor lista


produtoLista :: Num a => [a] -> a
produtoLista = foldr (*) 1

tamanhoLista :: Num a => [a] -> a
tamanhoLista = foldr (\_ n -> 1+n) 0

-- Retorna uma tupla com o indice do elemento e seu valor
zipar :: Num a => [a] -> [(Int,a)]
zipar ls = [(i,x)| (i,x) <- zip [0..n] ls]
            where 
                n = length ls - 1

subtrairAntecessor:: Int -> Float
subtrairAntecessor x = fromIntegral (x - (x-1))

--[0,1,4,9] -> [0.0,1.0,2.0,3.0]
converte1 :: [Int] -> [Float]
converte1 ls = map (\(x,_) -> fromIntegral x) (zipar ls)

--"HAL" -> "IBM"
converte2 :: String -> String
converte2 ls = map (\c -> succ c) ls

--["bom","dia","turma"] -> "bdt"
converte3 :: [String] -> String
converte3 ls = map (\(c:_) -> c) ls

--["ciência", "da", "computação"] -> [7,2,10]
converte4 :: [String] -> [Int]
converte4 ls = map(\x -> length x) ls

type Livro = String
type Database = [(Pessoa,[Livro])]

dataBase :: Database
dataBase = [(("Jorgin",18,75,"Futebol"),["L1","L2","L3"]),(("Mafalda",28,75,"Halterofilismo"),["L4","L5","L6","L1"]),(("cuzin",12,55,"Natação"),["L7","L8","L9"]),(("peludin",33,95,"Powerlifting"),["L10","L11","L12","L1"])]
                

emprestadosPorPessoa :: Database -> Pessoa -> [Livro]
emprestadosPorPessoa db pessoa = concatMap (\(p,livros) -> if p == pessoa then livros else []) db

emprestadosPorLivro :: Database -> Livro -> [Pessoa]
emprestadosPorLivro db livro = [pessoa | (pessoa,livros) <- db, livro `elem` livros]

estaEmprestado :: Database -> Livro -> Bool
estaEmprestado db livro = if True `elem` map (\(pessoa,livros) -> if livro `elem` livros then True else False) db then True else False

emprestimoPorPessoa:: Database -> Pessoa -> Int
emprestimoPorPessoa db pessoa = length (concatMap (\(p,livros) -> if pessoa == p then livros else []) db) 

procurarPessoa :: Database -> Pessoa -> Maybe (Pessoa,Int)
procurarPessoa db pessoa = case result of 
                                [] -> Nothing
                                (p,livros):_ -> Just(p,length livros)
                           where
                             result = [(p,livros) | (p,livros) <- db,p == pessoa]

-- IO
main :: IO()
main = do putStrLn ("Qual o seu nome? ")
          nome <- getLine
          putStrLn ("Qual sua matricula ?")
          mart <- getLine
          putStrLn ("Bem-vindo " ++ nome ++ " matricula " ++ mart)


calcular :: IO()
calcular = do putStr "Digite um numero: "
              n <- getLine
              let x = ((read n)::Double)
                  s = sin x
                  c = cos x
              putStr ("O seno de "++n++" e' "++(show s)++['.','\n']++
                      "O cosseno de "++n++" e' "++(show c)++['.','\n'])


-- Ordenar uma lista
sort [] = []
sort (a:b) = sort [x | x <- b, x<a]
             ++ [a] ++
             sort [x | x <- b, x>= a]

--Exercicio 8

palavraNaoTemNum :: String -> Bool
palavraNaoTemNum [] = True
palavraNaoTemNum (x:xs) | isDigit x = False
                        | otherwise = palavraNaoTemNum xs


inverterPalavra :: String -> String
inverterPalavra [] = []
inverterPalavra (x:xs) = (inverterPalavra xs) ++ [x]

contemApenasCaracteres :: IO()
contemApenasCaracteres = do putStr ("Digite uma palavra: ")
                            palavra <- getLine
                            if palavraNaoTemNum palavra then putStrLn (inverterPalavra palavra)
                            else error "Palavra tem caractere numerico"

pegaNumeTelefone:: IO()
pegaNumeTelefone = do putStr ("Digite seu nome: ")
                      nome <- getLine
                      putStr ("Digite seu telefone: ")
                      telefone <- getLine
                      putStrLn("Nome:  " ++ nome ++ ", telefone: " ++ telefone)


leLinhaAteVirVazio :: IO()
leLinhaAteVirVazio = do putStr ("Digite uma palavra, ou, para sair, digite nada e de enter: ")
                        palavra <- getLine
                        if palavra /= "" then palavras = leLinhaAteVirVazio
                        else
                            putStrLn ()