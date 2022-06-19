-- a) (2 pts) Que va afficher le code suivant :
t = [0, 4..30]
--main = print ( t )

-- t = [0, 4, 8, 12, 16, 20, 24, 28, 30]




-- b) (4 pts) Que va afficher le code suivant ?
t' = [0, 4..30]
p1 = filter ( \x -> x <= 10 )
m1 = map ( \x -> if x <= 10 then x else x - 10 )
--main = print ( ( p1 . m1 ) t )


-- t' = [0, 4, 8, 12, 16, 20, 24, 28, 30]
-- t' = [0, 4, 8, 2, 6, 10]





-- c) (4 pts) Que va afficher le code suivant ?

p1' = filter ( \x -> x <= 10 )
m1' = map ( \x -> if x <= 10 then x else x - 10 )
s f p xs = map ( \m -> ( p1 . m1 ) m ) xs
t'' = [0, 4..30]
u = [22, 24, 26, 28]
v = [1, 3, 15]
w = [t'', u, v]
main = print ( s m1' p1' w )

-- t'' = [0, 4, 8, 12, 16, 20, 24, 28, 30]
-- u = [22, 24, 26, 28]
-- v = [1, 3, 15]
-- w = [t'', u, v]
-- w = [[0, 4, 8, 2, 6, 10], [], [1, 3, 5]]