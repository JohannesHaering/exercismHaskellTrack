convertFromTo num from to = convertFromDecimals (convertToDecimal num from) to

getDigits 0 = []
getDigits num = mod num 10 : getDigits (div num 10)

convertFromDecimals num to = helper num to  
  where 
    helper num to 
      | num /= 0 = (mod num to) + 10 * helper (div num to) to  
      | otherwise = 0 

convertToDecimal num from = foldl (+) 0 (mapToDecimals digits 0 from)
  where digits = getDigits num

mapToDecimals [] _ _ = []
mapToDecimals (x:xs) pos from =  (x * from ^ pos) : mapToDecimals xs (pos + 1) from 
