--createNumbers :: int -> int -> int -> [int]
createNumbers counter max multiplier
    | newVal >= max = []
    | otherwise = newVal : (createNumbers (counter + 1) max multiplier)  
        where newVal = counter * multiplier

--merge :: [int] -> [int] -> [int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

--mergeMultiples :: [int] -> [[int]] -> [int]
mergeMultiples y (x:xs)
    | xs == [] = merge x y
    | otherwise = mergeMultiples (merge x y) xs

filterDupletes filtered (x:xs)
    | xs == [] = if elem x filtered then filtered else x : filtered
    | elem x filtered = filterDupletes filtered xs
    | otherwise = filterDupletes (x:filtered) xs

sumMultiples [] _ = 0
sumMultiples _ 0 = 0
sumMultiples xs y = foldr (+) 0 numbers
    where numbers = filterDupletes [] (mergeMultiples [] numbersArray)
          numbersArray = map (createNumbers 1 y) xs 
