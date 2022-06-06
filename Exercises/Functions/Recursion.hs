-- 1.1.1
-- Écrivez le code pour la fonction de Fibonacci est utilisant des gardes. Écrivez une deuxième version en
-- utilisant les patrons. Vous pouvez sous-entendre qu’il n’y aura pas de valeur négative en entrées.
-- Signature : fibo :: Int -> Int

fibo :: Int -> Int
fibo 0 =0
fibo 1 =1
fibo x = fibo (x-1) + fibo(x-2)

-- Il est possible de construire une version plus générale de Fibonacci, en redéfinissant les deux premières
-- valeurs. Construisez une version plus générale de la fonction de Fibonacci, qui accepte les deux
-- premières valeurs en argument.
-- Signature : fiboG :: Int -> Int -> Int -> Int
fiboG :: Int -> Int -> Int -> Int
fiboG v1 v2 0 = v1
fiboG v1 v2 1 = v2
fiboG v1 v2 n = fiboG v1 v2 (n-1) + fiboG v1 v2 (n-2)

-- 1.1.3
-- Construisez une nouvelle version de la fonction de Fibonacci en utilisant la fonction définie au numéro
-- précédant.

fibo' :: Int -> Int
fibo'=fiboG 0 1

-- 1.1.4
-- Écrivez une fonction qui construit une liste contenant les n premiers nombre de Fibonacci à partir de 0.
-- Signature : listeNFibo :: Int -> [Int]
-- Exemple : listeNFibo 4 == [0, 1, 1, 2]

listeNFibo :: Int -> [Int]
listeNFibo 0 = [0]
listeNFibo x = listeNFibo (x-1) ++ [fibo x] 