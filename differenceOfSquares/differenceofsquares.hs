differenceInSquares n = ((foldr (+) 0 [1..n]) ^ 2) - (foldr (+) 0 (map (\x -> x ^ 2) [1..n]))
