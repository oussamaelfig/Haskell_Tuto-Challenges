-- Que va afficher le code suivant :
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

data C a = S | K | V a | B [C a]
type P a = [C a]


f :: P a -> [a]
f [] = []
f (V x : xs) = x : ( f xs )
f (K : x : y : xs) = f (x:xs)
f (S : x : y : z : xs) = f ((B [x, z]) : (B [y, z]) : xs)
f (B xs : ys) = f (xs ++ ys)
f _ = error "manque de C"


main = print ( f [S, K, K, V 2] )

-- f [S, K, K, V 2]
        -- f ((B [x, z]) : (B [y, z]) : [])
-- f ((B [K, V 2]) : (B [K, V 2]) : [])
        -- f (B xs : ys) = f (xs ++ ys)
-- f ([K, V 2] ++ (B [K, V 2]) : [])
        -- f (K : x : y : xs) = f (x:xs)
-- f ( [K, V 2, B [ K, V 2 ] ] )
        -- f (K : x : y : xs) = f (x:xs)
-- f(2 : [])
-- [2]


