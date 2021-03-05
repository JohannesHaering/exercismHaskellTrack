import Data.Char
import Data.String

toLowerString :: [Char] -> [Char]
toLowerString str = [ toLower x | x <- str]


removeItem x [] = []
removeItem x (y:ys) 
    | x == y = removeItem x ys
    | otherwise = y : removeItem x ys


addItem :: [Char] -> Char -> [Char]
--addItem xs "" = xs
addItem [] x = [x]
addItem (y:ys) x
    | x == y = y:ys
    | otherwise = y : addItem x ys

--isPanagram :: [Char] -> Bool
isPanagram sentence = do { let alphabet = ['a'..'z']
; let chars = [] :: [Char]
; foldr addItem chars sentence
--;return  ((length alphabet) == 0) :: Bool
}
