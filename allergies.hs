-- convert to binary
-- split into digits in reversed order
-- determine the allergies

convertToBinary 0 = 0
convertToBinary 1 = 1
convertToBinary num
  | ((mod num 2) == 1) = 1 + 10 * (convertToBinary (div num 2))
  | otherwise = 10 * (convertToBinary (div num 2))

getScore 0 = []
getScore 1 = [1]
getScore num
  | ((mod num 2) == 1) = 1 : (getScore (div num 2))
  | otherwise = 0 : (getScore (div num 2))

getPosTranslation 0 = "eggs"
getPosTranslation 1 = "peanuts"
getPosTranslation 2 = "shellfish"
getPosTranslation 3 = "strawberries"
getPosTranslation 4 = "tomatoes"
getPosTranslation 5 = "chocolate"
getPosTranslation 6 = "pollen"
getPosTranslation 7 = "cats"

getAllergies 0 = []
getAllergies score = helper list 0
  where 
    list = getScore score
    helper [] _ = []
    helper (x:xs) pos
      | pos > 7 = []
      | x == 0 = helper xs (pos+1)
      | otherwise = (getPosTranslation pos) : (helper xs (pos+1))
    
