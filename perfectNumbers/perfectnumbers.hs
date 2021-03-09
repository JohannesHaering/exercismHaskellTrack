aliquotSum num = foldr (+) 0 (filter (\x -> mod num x == 0) [1..num - 1])

isPerfectNumber num = (aliquotSum num) == num
isAbundantNumber num = (aliquotSum num) > num
isDeficientNumber num = (aliquotSum num) < num


getPerfectNumbers = filter isPerfectNumber [1..]
getAbundantNumbers = filter isAbundantNumber [1..]
getDeficientNumbers = filter isDeficientNumber [1..]
