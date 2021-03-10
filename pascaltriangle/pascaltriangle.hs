pascalTriangle depth = foldl (\x y -> x ++ [(getNextLine x)]) [[]] [1..depth]

getNextLine xs = 1 : helper (last xs)
  where
    helper [] = []
    helper (x:[]) = [1]
    helper (x:y:[]) = (x+y) : helper (y:[])
    helper (x:y:xs) = (x+y) : helper (y:xs) 

