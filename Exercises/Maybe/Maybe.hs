-- 4.2.1
-- Écrivez une fonction qui permet d’invalider une valeur en utilisant le Monad Maybe. La fonction prend
-- un prédicat et une valeur. Lorsque le prédicat appliqué à la valeur donne True, alors la valeur
-- encapsulée dans un Just est retournée. Sinon, la fonction retourne Nothing.à

assert :: (a -> Bool) -> a -> Maybe a
assert f x  | f x = Just x
            | otherwise=Nothing


-- 4.2.2
-- Écrivez une fonction qui calcule la somme des valeurs de 1 à ‘n’ si ‘n’ est plus grand que 0, dans ce cas
-- c’est le résultat est retourné dans un ‘Just’. Sinon, la fonction retourne Nothing. Pour cela, utilisez
-- ‘assert’ du numéro précédant.

somme :: Int -> Int
somme n = sum [1..n]


sommeM :: Int -> Maybe Int

sommeM a | a<0 = Nothing
         | a==0 = Nothing
         | a>0 = Just (somme a)


sommeM' :: Int -> Maybe Int
sommeM' n = fmap somme (assert (0 <) n)

-- 4.2.3
-- Écrivez une fonction qui reçoit un Maybe, si c’est une Just, alors elle affiche la valeur encapsulée, sinon
-- elle affiche un message d’erreur.
affiche :: (Show a) => Maybe a -> String -> IO ()
affiche a mssg = do putStrLn (maybe mssg show a )