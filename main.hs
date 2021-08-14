{- 
  TP3 - Lista de atividades de Haskell
  Aluno: Jonathan Douglas Diego Tavares
  Matrícula: 201622040228
-}

import Data.List

--Potência(1)
potencia :: Int -> Int -> Int
potencia a b
 | b == 0 = 1
 | b > 0 = a * potencia a (b-1)

 --SomaImpares(2)
somaImpar :: [Int] -> Int
somaImpar [] = 0
somaImpar (a:x) 
 | a `mod` 2 == 0 = somaImpar x
 | otherwise = a + somaImpar x

--Substituir(3)
substituir :: Int -> Int -> [Int] -> [Int]
substituir c d [] = []
substituir c d (a:x)
 | a == c = d: substituir c d x 
 | a /= c = a: substituir c d x 

--Primo(4)
--calcula os divisores de n 
fatores :: Int-> [Int]
fatores n = [i | i<-[1..n], n `mod` i == 0]

primo :: Int -> Bool
primo n
 |n == 1 = True
primo n = if (fatores n) == [1,n]   then True
  else False

--Perfeito(5)
--calcula os divisores até n - 1
divisores :: Int-> [Int]
divisores n = [i | i<-[1..n-1], n `mod` i == 0]

--soma todos os elementos de uma lista
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:x)
 |a /= 0 = a + somaLista x

perfeito :: Int -> Bool
perfeito n = if (somaLista (divisores n)) == n then True
  else False

--Binario(6)
binario :: Int -> [Int]
--casos triviais
binario 0 = [0]
binario 1 = [1]
--calcula resto de n divido por 2 a cada recursão
--simulando divisão sucessiva na mão
binario n
 | mod n 2 == 0 = binario (div n 2) ++ [0]
 | otherwise = binario (div n 2) ++ [1]

--Distintos(7)
--calcula a ocorrencia de um numero em uma lista
ocorrencia :: Int -> [Int] -> Int
ocorrencia n [] = 0
ocorrencia n (a:x)
 |n == a = 1 + ocorrencia n x
 |n /= a = ocorrencia n x

distintos :: [Int] -> Bool
distintos [] = True
distintos (a:x) 
 |ocorrencia a x == 1 = False
 |otherwise = distintos x

--conta o tamanho de uma lista
contar :: [Int] -> Int
contar [] = 0
contar (c : r) = 1 + contar r

--Disjuntas(8)
disjuntas :: [Int] -> [Int] -> Bool
disjuntas a b
 |contar a /= contar b = True
 |a == b = False
 |a /= b = True


{-
disjuntas :: [Int] -> [Int] -> Bool
disjuntas [] [] = True
disjuntas _ [] = False
disjuntas [] _ = False
disjuntas x y 
 |contar x /= contar y = False

disjuntas (a:x) (b:y)
 |a == b = disjuntas x y
 |a /= b = False
-}

--Palindromo(9)
--calcula a ordem reversa de uma lista
reverso :: [Int] -> [Int]  
reverso [] = []
reverso (x:xs) = reverso (xs) ++ [x]

palindromo :: [Int] -> Bool
--verifica a disjunção entre a lista reversa e a lista inicial  
palindromo (x:xs) = if ((x:xs) == (reverso (x:xs))) then True else False

--SomasParciais(10)
--Soma os n primeiros numeros de uma lista
somaN :: Int -> [Int] -> Int
somaN _ [] = 0
somaN 0 _ = 0
somaN n (a:x)
 |n > 0 = a + somaN (n-1) x

--Soma 2 numeros e devolve o resultado em uma lista
somaL :: Int -> Int -> [Int]
somaL 0 a = [a]
somaL a 0 = [a]
somaL a b
 |a == b = [2*a]
 |a /= b = [a+b]

--calcula somas parciais usando a função scanl1 que recebe como
--parâmetro a operação a ser realizada com os elementos da lista
somaParciais :: [Int] -> [Int]
somaParciais [] = []
somaParciais n = scanl1 (+) n
  
--Linearizar(11)
linearizar :: [[Int]]->[Int]
linearizar [] = []
linearizar (a:x) = a ++ linearizar x

--Shift(12)
--adiciona elemento no fim da lista
adicionaFim :: Int -> [Int] -> [Int]
adicionaFim a [] = [a]
adicionaFim a (x:xs) = x : adicionaFim a xs

shift :: Int -> [Int] -> [Int]
shift 0 a = a
shift n (a:x)
 |n > 0 = shift (n-1) (adicionaFim a x)

--RemoverFim(13)

{-
Versão com take (funcional - só de exemplo que também existe)

removerFimT :: Int -> [Int] -> [Int]
removerFimT 0 x = x
removerFimT _[] = []
removerFimT n x
 | n > contar x = error "Tamanho inválido"
 | n > 0 = take (contar x - n) x
-}

--Versão recursiva (usada no main)

--pega os n primeiros elementos do inicio da lista
pegaInicio :: Int -> [Int] -> [Int]
pegaInicio 0 x = x
pegaInicio _[] = []
pegaInicio n (a:x)
 |n == contar x+1 = [a] ++ x
 |n > 1  = [a] ++ pegaInicio (n-1) x
 |otherwise = [a]

removerFim :: Int -> [Int] -> [Int]
removerFim 0 x = x
removerFim _[] = []
removerFim n x
 | n == contar x = []
 | n > contar x = error "Tamanho inválido"
 | n > 0 = pegaInicio (contar x - n) x 

 
--Intercalar(14)

--Ord contido na biblioteca Data.Ord
--permite a comparação entre 2 elementos
--para que se possa determinar a sua ordem
intercalar :: Ord a => [a] -> [a] -> [a]
--casos triviais
intercalar xs [] = xs
intercalar [] xs = xs
--operador @ significa "leia como
--para casamento de padrão"
intercalar a@(h:primeiro) b@(c:segundo)
 | h <= c = h:intercalar primeiro b
 | h > c = c:intercalar a segundo

--Trocar(15)
--subtrai dois numeros
subtrair :: Int -> Int -> Int
subtrair a b
 |a == b = 0
 |a /= b = a - b

--Devolve o troco da maior nota para a menor
trocar :: Int -> [Int]
trocar 0 = []
trocar n
 |n > 100 = [100] ++ trocar (n-100)
 |n == 100 = [100] ++ trocar (n-100)
 |n > 50 = [50] ++ trocar (n-50)
 |n == 50 = [50] ++ trocar (n-50)
 |n > 10 = [10] ++ trocar (n-10)
 |n == 10 = [10] ++ trocar (n-10)
 |n > 5 = [5] ++ trocar (n-5) 
 |n == 5 = [5] ++ trocar (n-5)
 |n > 1 = [1] ++ trocar (n-1) 
 |n == 1 = [1] ++ trocar (n-1)

main=do
  putStrLn "Exercício 1"
  print $ potencia 2 4
  putStrLn " "

  putStrLn "Exercício 2"
  print $ somaImpar [1,2,3,4,5,6,7] 
  putStrLn " "
  
  putStrLn "Exercício 3"
  print $ substituir 1 0 [1,5,7,9,1]
  putStrLn " "
  
  putStrLn "Exercício 4"
  print $ primo 24
  print $ primo 23
  putStrLn " "
  
  putStrLn "Exercício 5"
  print $ perfeito 28
  print $ perfeito 27
  putStrLn " "
  
  putStrLn "Exercício 6"
  print $ binario 1
  print $ binario 2
  print $ binario 4
  print $ binario 8
  print $ binario 15
  print $ binario 16
  putStrLn " "
  
  putStrLn "Exercício 7"
  print $ distintos [1,2,4,2,5]
  print $ distintos [3,2,1]
  putStrLn " "
  
  putStrLn "Exercício 8"
  print $ disjuntas [1,2,3,4] [1,2,3,4]
  print $ disjuntas [1,2,3,4] [1,2,3,5]
  print $ disjuntas [1,2,3,4] [1,2,3]
  putStrLn " "
  
  putStrLn "Exercício 9"
  print $ palindromo [1,2,3,4,3,2,1]
  print $ palindromo [1,2,3,4,5,2,1]
  print $ palindromo [1,2,2,1]
  print $ palindromo [1,2,3,4]
  putStrLn " "

  putStrLn "Exercício 10"
  print $ somaParciais [1,2,3,4]
  print $ somaParciais [4,3,2,1]
  putStrLn " "

  putStrLn "Exercício 11"
  print $ linearizar [ [1,2], [5], [0,4,2], [42,50] ]
  putStrLn " "

  putStrLn "Exercício 12"
  print $ shift 3 [1,5,6,7,3,4,1]
  putStrLn " "

  putStrLn "Exercício 13"
  print $ removerFim 1 [1,2,3,4,5,6]
  print $ removerFim 2 [1,2,3,4,5,6]
  print $ removerFim 3 [1,2,3,4,5,6]
  print $ removerFim 4 [1,2,3,4,5,6]
  print $ removerFim 5 [1,2,3,4,5,6]
  print $ removerFim 6 [1,2,3,4,5,6]
  putStrLn " "

  putStrLn "Exercício 14"
  print $ intercalar  [1,5,10] [2,7,9,20,25]
  putStrLn " "

  putStrLn "Exercício 15"
  print $ trocar 162