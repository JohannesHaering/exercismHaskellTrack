getDigits 0 = []
getDigits num = mod num 10 : getDigits (div num 10)

largest series n fun = helper digits (getArrayOfSize (n-1)) 0
  where
    helper (x:xs) accs maxProd
      | xs == [] = max maxProd (fun (last accs) x)
      | otherwise = helper xs (updateAccs accs x 1 fun) (max maxProd (fun (last accs) x))
    digits = reverse (getDigits series)
    updateAccs (acc:accs) x temp fun
      | accs == [] = [fun temp x]
      | otherwise = (fun temp x) : (updateAccs accs x acc fun)

largestProduct series n = largest series n (*)

largestSum series n = largest series n (+)

largestDifference series n = largest series n (-)

getArrayOfSize n
  | n <= 0 = []
  | otherwise = 0 : (getArrayOfSize (n-1))  
     
