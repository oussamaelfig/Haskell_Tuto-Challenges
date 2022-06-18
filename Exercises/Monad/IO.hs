import Data.Functor.Sum (Sum(InR))
import Text.Printf (printf)
-- 4.1.1
-- Expliquez ce que fait le code suivant.
s :: IO Bool -> IO () -> IO () -> IO ()
s p i1 i2 = do t <- p
               if t then do i1
                            return ()
                    else do i2
                            return ()
main = do r <- getLine
          s ( return ( ( head r ) == '1' ) )
            ( putStrLn ( tail r ) )
            ( putStrLn ( reverse r ) )
-- essayez le code avec différentes entrées au clavier : 1537, 5431…


-- 4.1.2 (utilisé pour les autres numéros)
-- Écrivez une fonction qui calcule la somme des valeurs de 1 à ‘n’.
somme :: Int -> Int
somme 0 = 0
somme a = a + somme (a-1)

-- Écrivez un code qui demande une valeur ‘n’ à un utilisateur et calcule la somme des valeurs de 1 à n et
-- ensuite affiche le résultat. Remarquez, si une constante ‘s’ contient une chaine de caractères
-- représentant un ‘Int’, il est possible de transformer cette chaine en Int en utilisant : ( read s ) :: Int
sommeEntree :: IO()
sommeEntree = do
    putStrLn "Entrer un nombre : "
    a <- getLine
    let nbr = (read a :: Int)
    let som = somme nbr
    print som