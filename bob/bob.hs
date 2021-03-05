import Data.Char

--isLastLetterQuestionMark :: [Char] -> Bool
lastLetterEqual (x:xs) y
    | xs == [] = (x == y)
    | otherwise = (lastLetterEqual xs y)

isLastLetterQuestionMark xs
    | xs == [] = False
    | otherwise = lastLetterEqual xs '?'

isLastLetterExamationMark xs
    | xs == [] = False
    | otherwise = lastLetterEqual xs '!' 

capitalAlphabet = ['A'..'Z']
lowerAlphabet = ['a'..'z']

--isCapital x:xs = ((filter (\x -> elem x capitalAlphabet) xs

isOnlyCapital sentence = (length (filter (\x -> elem x lowerAlphabet) sentence)) == 0 


answer :: [Char] -> [Char]
answer sentence 
    | sentence == "Bob" = "Fine. Be that way!"
    | (isOnlyCapital sentence) && (isLastLetterQuestionMark sentence) = "Calm down, I know what I'm doing!" 
    | (isOnlyCapital sentence) && (isLastLetterExamationMark sentence) = "Whoa, chill out!"
    | (isLastLetterQuestionMark sentence) = "Sure."
    | otherwise = "Whatever"
