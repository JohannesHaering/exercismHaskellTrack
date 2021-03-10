convertToRoman num
    | num > 3000 = "" 
    | num <= 0 = ""
    | otherwise = getThousands num ++ getHundreds num ++ getTens num ++ getOnes num 


getOnes num = getConversion ones 'I' 'V' 'X'
  where ones = div (mod num 10) 1

getTens num = getConversion tens 'X' 'L' 'C'
  where tens = div (mod num 100) 10

getHundreds num = getConversion hundreds 'C' 'D' 'M'  
  where hundreds = div (mod num 1000) 100

--getConversion :: Num a => a -> Char -> Char -> Char -> [Char]
getConversion num lower middle higher
    | num == 0 = ""
    | num <= 3 = getNChars lower num
    | num == 4 = lower : [middle]
    | num == 5 = [middle]
    | num <= 8 = middle : (getNChars lower (num-5))
    | num == 9 = lower : [higher]
    | otherwise = ""

getThousands num = getNChars 'M' (div num 1000)

getNChars char 1 = [char]
getNChars char n = char : (getNChars char (n-1)) 
