app [] xs = xs
app (x:xs) ys = x : (app xs ys)

concatenate [] = []
concatenate (x:xs) = app x (concatenate xs)

filt fun [] = []
filt fun (x:xs)
    | fun x = x : (filt fun xs)
    | otherwise = filt fun xs

mapper fun [] = []
mapper fun (x:xs) = (fun x) : (mapper fun xs)

len [] = 0
len xs = lengthHelper xs
lentghHelper [] = 0
lengthHelper (x:xs) 
    | xs == [] = 1
    | otherwise = 1 + (lengthHelper xs)


foldler fun acc [] = acc
foldler fun acc (x:xs)
    | xs == [] = fun acc x
    | otherwise = foldrer fun (fun acc x) xs

foldrer fun acc [] = acc
foldrer fun acc (x:xs)
    | xs == [] = fun acc x
    | otherwise = fun x (foldrer fun acc xs)

rev xs = revHelper xs []
    where
      revHelper [] acc = acc
      revHelper (x:xs) acc = revHelper xs (x:acc)   
