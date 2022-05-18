
-- quicksort [] = []
-- quicksort (x:xs) = quicksort small ++ (x : quicksort large)
--    where small = [y | y <- xs, y <= x]
--          large = [y | y <- xs, y > x]

main :: IO ()
main = putStrLn  "Hello world"
 