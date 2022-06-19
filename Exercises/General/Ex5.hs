-- Que va afficher le code suivant :
i :: (Ord a) => (a, a) -> a -> (a, a)
i (x, y) z = (x1, y2)
    where
        x1 = min x z
        y2 = max y z
mm :: (Ord a) => [a] -> Maybe (a,a)
mm [] = Nothing
mm (x:xs) = Just (foldl i ( x, x ) xs)

r = [7, 3, 5, 8, 1, 4, 6, 3]

main = print ( mm r )

-- Notez que :

-- instance Show a => Show Maybe a where
--         show Nothing = "Nothing"
--         show (Just x) = "Just " ++ (show x)


-- Just (1,8)