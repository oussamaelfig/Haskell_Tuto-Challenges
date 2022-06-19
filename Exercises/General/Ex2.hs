-- Soit le code suivant :
f1 :: (t1 -> t2 -> t3) -> (t1 -> t4 -> t2) -> t1 -> t4 -> t3
f1 x y s z = x s ( y s z )


c :: (Num t1, Ord t1) => t1 -> (t2 -> t2) -> t2 -> t2
c 0 _ z = z
c n s z | n > 0 = s ( c ( n - 1 ) s z )
        | otherwise = error "seul les valeurs positives sont acceptees."


a :: (t2 -> t2) -> t2 -> t2
a = c 3


b :: (t2 -> t2) -> t2 -> t2
b = c 2


main :: IO ()
main = print ( f1 a b (1+) 0 )

-- f1 a b (1+) 0
        -- x=a  y=b     s=(1+)  z=0
-- x s ( y s z )
        -- a (1+) (b (1+) 0)
-- c n s z | n > 0 = s ( c ( n - 1 ) s z )
        -- c 3 (1+) (b (1+) 0)
                -- n=3  s=(1+)  z=(b (+1) 0)
        -- (1+) ( c (2) (1+) (b (+1) 0))
                -- n=2  s=(1+)  z=(b (+1) 0)
        -- (1+) ( (1+) (c (1) (1+) (b (+1) 0)))
                -- n=1  s=(1+)  z=(b (+1) 0)
        -- (1+) ( (1+) ((1+) (c(0) (1+) (b (+1) 0)))
                -- c 0 _ z = z
                -- n=0  s=(1+)  z=(b (+1) 0)
        -- (1+) ( (1+) ((1+) (b (+1) 0))
        -- (1+) ( (1+) ((1+) (c (2) (+1) 0))
                -- n=2  s=(1+)  z=0
        -- (1+) ( (1+) ((1+) ((1+) c(1) (1+) 0))
                -- n=1  s(1+)   z=0
        -- (1+) ( (1+) ((1+) ((1+) ((1+) c(0) (1+) 0))
                -- n=0  s(1+)   z=0
                -- c 0 _ z = z
        -- (1+) ( (1+) ((1+) ((1+) ((1+) (0))
        -- 5


--  Que va afficher le code si nous modifions le ’main’ comme suit :
-- main = print ( f1 a b ('*':) "" )
-- *****


 









-- f1 (c 3) (c 2) (1+) 0
-- f1 ( (1+) (c (3-1) (1+) 0))  ( (1+) (c (2-1) (1+) 0)) (1+) 0
-- f1 