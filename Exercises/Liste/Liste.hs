-- 2.1.1
-- Écrivez une fonction (repeter n x) qui construit une liste contenant n fois un élément x.

repeter :: Int -> a -> [a]
repeter 0 _ = []
repeter k x
  | k > 0 = x : repeter (k -1) x
  | otherwise = error "le nombre est negatif"


--   2.1.2
-- Écrivez une fonction qui multiplie chaque élément d’une liste par une constante. Utilisez une liste en
-- compréhension pour ce code.

mulK :: Int -> [Int] -> [Int]
mulK _ [] = []
mulK k (x:xs) = k*x : mulK k xs 


-- 2.1.3
-- Faites la trace de l’appel de la fonction reverse.
-- foldl _ z [] = z
-- foldl f z ( x : xs ) = foldl f (f z x) xs
-- flip f x y = f y x
-- reverse = foldl (flip (:)) []
-- reverse [2,4,6]



-- 2.1.3
-- reverse [2,4,6]
-- foldl (flip (:)) [] [2,4,6]
-- foldl (flip (:)) ((flip (:)) [] 2) [4,6]
-- foldl (flip (:)) ((flip (:)) ((flip (:)) [] 2) 4) [6]
-- foldl (flip (:)) ((flip (:)) ((flip (:)) ((flip (:)) [] 2) 4) 6) []
-- ((flip (:)) ((flip (:)) ((flip (:)) [] 2) 4) 6)
-- ((flip (:)) ((flip (:)) ((:) 2 []) 4) 6)
-- ((flip (:)) ((flip (:)) [2] 4) 6)
-- ((flip (:)) ((:) 4 [2]) 6)
-- ((flip (:)) [4, 2] 6)
-- ((:) 6 [4, 2])
-- [6, 4, 2]





-- 2.1.4
-- Écrivez une fonction qui prend les éléments d’une liste tant qu’il donne vrai pour le prédicat donné.
prendre :: ( a -> Bool ) -> [a] -> [a]
prendre _ []= []
prendre k (x:xs) | k x = x : prendre k xs 
                 | otherwise = []


-- 2.1.5
-- Écrivez une fonction qui enlève toutes les copies d’un élément dans une liste. Utilisez une liste en
-- compréhension.
supprimer :: Int -> [Int] -> [Int]
supprimer _ [] = []
supprimer k (x:xs) | k == x = supprimer k xs
                   | otherwise = x : supprimer k xs


-- 2.1.6
-- Écrivez une fonction qui enlève les éléments en double d’une liste. (Pour chaque valeur, seule la
-- première occurrence est conservée.) Réutiliser la fonction supprimer du numéro précédant.

distinct :: [Int] -> [Int]
distinct [] = []
distinct (x:xs) = x : distinct(supprimer x xs)


-- 2.1.7
-- Faites la trace suivante.

d :: (t1 -> t1 -> t2) -> t1 -> t2
d f x = f x x
main = print ( ( d (.) ) ( d (+) ) 1 )

-- ( ( d (.) ) ( d (+) ) 1 )
-- ( (.) ( d (+) ) ( d (+) ) 1 )
-- ( d (+) ) (+) 1 1
-- ( d (+) ) 2
-- (+) 2 2
-- 4


