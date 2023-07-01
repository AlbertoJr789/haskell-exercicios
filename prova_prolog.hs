--1 Ve se a solução do cara resolve problema de coloração dos grafos.
import Data.List (nub)

type Vertice = Integer
type Vizinho = Vertice
type Cor = String

type Grafo = [(Vertice,Vizinho)]
type Solucao = [(Vertice,Cor)]

grafoEx :: Grafo
grafoEx = [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(4,5),(5,4),(5,6),(6,5),(6,1),(1,6)]

resolveColoracao :: Solucao -> Grafo -> Bool
resolveColoracao [] _ = True
resolveColoracao sol@(s:rs) grafo | (checarVizinhos s sol grafo) == False = False
                                  | otherwise = resolveColoracao rs grafo

checarVizinhos :: (Vertice,Cor) -> Solucao -> Grafo -> Bool
checarVizinhos _ _ [] = True
checarVizinhos (vertice,corV) solucao ((vert,vizinho):gs) | vertice == vert = --estou olhando o vertice correto no grafo  
                                                            if  corV == (cor solucao vizinho) 
                                                                then False
                                                            else
                                                               checarVizinhos (vertice,corV) solucao gs                                         
                                                          | otherwise = checarVizinhos (vertice,corV) solucao gs

cor :: Solucao -> Vertice -> Cor
cor [] vertice = ""
cor ((vert,corV):rs) vertice | vertice == vert = corV
                             | otherwise = cor rs vertice


-- 2 inverter uma lista
inverterLista :: [a] -> [a]
inverterLista [] = []
inverterLista (x:xs) = (inverterLista xs) ++ [x]

type Dia = String
dias :: [Dia]
dias = ["segunda","terca","quarta","quinta","sexta","sabado","domingo"]

type Estudante = String
estudantes :: [Estudante]
estudantes = ["Alex","Bruno","Carlos","Daniel"]

diasLivres:: [(Estudante,[Dia])]
diasLivres = [("Alex",["segunda","terca"]),("Bruno",["segunda","quarta","quinta"]),("Carlos",["terca","quarta"]),("Daniel",["segunda","quinta","sexta"])] 

diasLivresEmComum :: [(Dia,[Estudante])]
diasLivresEmComum = [(d, nub [estudante | (estudante,dias) <- diasLivres, d `elem` dias]) | d <- dias]

diasLivresEmComumNaRaca :: [(Dia,[Estudante])]
diasLivresEmComumNaRaca = [(d,estudantesComum d) | d <- dias]
estudantesComum dia = [estudante | (estudante,dias) <- diasLivres, dia `elem` dias]

diasEstudante :: Estudante -> [Dia]
diasEstudante nome = concat([dias | (estudante,dias) <- diasLivres, estudante == nome])

-- Questao 5, verifica se lista ta ordenada
verificaListaOrdenada :: Ord a => [a] -> Bool
verificaListaOrdenada [] = True
verificaListaOrdenada (x:[]) = True
verificaListaOrdenada (x:y:xs) | x < y = verificaListaOrdenada xs
                               | otherwise = False
-- Questao 6, retorna maior numero da lista
maiorNum :: Ord a => [a] -> a

maiorNum (x:[]) = x
maiorNum lista = pegarMaior lista (head lista)

pegarMaior :: Ord a => [a] -> a -> a
pegarMaior [] numAtual = numAtual
pegarMaior (x:xs) numAtual | x < numAtual = pegarMaior xs numAtual
                           | otherwise = pegarMaior xs x 
