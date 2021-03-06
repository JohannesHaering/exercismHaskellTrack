valid x = (x == 'G' || x == 'C' || x == 'T' || x == 'A' )
notValid x = not (valid x)


firstInvalid [] = '\00'
firstInvalid (x:xs)
    | xs == [] && valid x = '\00'
    | not (valid x) = x 
    | otherwise = firstInvalid xs

transcriber x
   | x == 'G' = 'C'
   | x == 'C' = 'G'
   | x == 'A' = 'T'
   | x == 'T' = 'A'

transcribe rna
    | rna == "" = ""
    | x /= '\00' = [x] 
    | otherwise = map transcriber rna 
        where x = firstInvalid rna

