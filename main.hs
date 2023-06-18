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
tamanhoStrings:: [String] -> (a -> a) -> [Int]
tamanhoStrings (h:xs) f x = [(f x)] : tamanhoStrings xs