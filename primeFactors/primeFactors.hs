getPrimeFactors num = filter (\x -> mod num x == 0) possiblePrimeFactors
  where
    possiblePrimeFactors = getPrimesWithMaximum num  

getPrimesWithMaximum max = filter isPrime [2..max]
getPrimes = filter isPrime [2..]
isPrime num = foldl (\x y -> x && (mod num y) /= 0) True [2..num-1]
