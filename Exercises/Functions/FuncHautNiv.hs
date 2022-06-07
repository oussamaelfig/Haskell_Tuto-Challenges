-- 1.2.1
-- Construisez une fonction récursive pour calculer la somme des éléments d’une liste. Vous ne pouvez
-- pas utiliser les fonctions prédéfinies suivantes : foldl, foldr, sum.

sommeListe :: [Double] -> Double
sommeListe [] = 0
sommeListe (x:xs) = x +  sommeListe xs


-- 1.2.2
-- Construisez une fonction qui reçoit en entrées un double (k) et une liste de double. Cette fonction va
-- premièrement calculer la k-ième puissance de chaque élément de la liste et faire la somme des
-- résultats. Peut-être utiliser la fonction du numéro 1.1.1? Vous ne pouvez pas utiliser foldl, foldr, sum,
-- map.

sommeK :: Double -> [Double] -> Double
sommeK k [] = 0
sommeK k (x:xs) = (x**k) + (sommeK k xs)


-- 1.2.3
-- Construisez une fonction qui calculer la norme d’ordre k d’une liste d’éléments. La norme consiste à
-- prendre la somme des k-ième puissance de chaque élément d’une liste et de calculer la k-ième racine de
-- ce résultat. Utiliser la fonction du numéro 1.1.2. Remarquez, calculer la k-ième racine d’une valeur
-- consiste à élever cette valeur à l’exposant 1/k

pNorme :: Double -> [Double] -> Double
pNorme k (x:xs) = (sommeK k (x:xs))**(1/k)


-- 1.2.4
-- Construisez une fonction qui calcule la norme euclidienne et une fonction qui calcule la norme de
-- Manhattan. La norme euclidienne est la norme d’ordre 2 et la norme de Manhattan est la norme
-- d’ordre 1. Définitivement, utilisez la fonction du numéro 1.1.3.

normeEuclidienne :: [Double] -> Double
normeEuclidienne (x:xs) = pNorme 2 (x:xs)

normeManhattan :: [Double] -> Double
normeManhattan (x:xs) = pNorme 1 (x:xs)

