getDigits 0 = []
getDigits num = mod num 10 : getDigits (div num 10)

largestProduct series n = helper digits (getArrayOfSize (n-1)) 0
  where
--    helper :: Num a => [a] -> [a] -> a -> a
    helper (x:xs) accs maxProd
      | xs == [] = max maxProd ((last accs) * x)
      | otherwise = helper xs (updateAccs accs x 1) (max maxProd ((last accs) * x))
    digits = reverse (getDigits series)
    updateAccs (acc:accs) x temp
      | accs == [] = [temp * x]
      | otherwise = (temp * x) : (updateAccs accs x acc)

getArrayOfSize n
  | n <= 0 = []
  | otherwise = 0 : (getArrayOfSize (n-1))  
     
