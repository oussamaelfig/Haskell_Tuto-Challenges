module Pangram (isPangram) where
import Data.Char
import Data.List (nub)
isPangram :: String -> Bool
-- "there should be 26 . elements in . the unique list of . elements filtered on (being between a & z) . from the inout where each element is ransformed to lower case
isPangram = (== 26) . length . nub . filter (\c -> c `elem` ['a'..'z']) .  map toLower