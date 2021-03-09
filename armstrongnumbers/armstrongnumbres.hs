getDigits 0 = []
getDigits num = mod num 10 : getDigits (div num 10)

reverseList xs = foldl (\acc x -> x : acc) [] xs

isArmstrongNumber num = (num == 0) || (num == armstrong)
    where armstrong = foldr (+) 0 (map (\x -> x ^ (length digits)) digits )
          digits = reverseList (getDigits num) 

getArmstrongNumbers = filter isArmstrongNumber [0..]
