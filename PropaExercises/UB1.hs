pow1 b 0 = 1
pow1 b e = b * (pow1 b (e-1))

pow2 b 0 = 1
pow2 b e 
  | mod e 2 == 0 = pow2 (b*b) (div e 2)
  | otherwise = b * pow2 b (e-1)

pow3 b e = pow3Helper b e 1
  where
    pow3Helper b 0 acc = acc
    pow3Helper b e acc
      | mod e 2 == 0 = pow3Helper (b*b) (div e 2) acc
      | otherwise = pow3Helper b (e-1) (b*acc)

root e r
  | r < 0 = -1
  | otherwise = intervallHalving e r 1 r

intervallHalving e r a b
  | b-a == 1 = b 
  | potence >= r = intervallHalving e r a m
  | otherwise = intervallHalving e r m b
    where 
      potence = pow3 m e
      m = div (a+b) 2

isPrime n = primeHelper n 2 (root 2 n)
  where 
    primeHelper n d t
      | d > t = True
      | mod n d  == 0 = False
      | otherwise = primeHelper n (d+1) t

insert [] y = y:[]
insert (x1:xs) y
  | x1 <= y = x1 : (insert xs y)
  | otherwise = (y:x1:xs)
insertionSort xs = insertionSortHelper xs []
  where
    insertionSortHelper [] acc = acc
    insertionSortHelper (x:xs) acc = insertionSortHelper xs (insert acc x)

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : (merge xs (y:ys))
  | otherwise = y: (merge (x:xs) ys)
mergeSort (x:[]) = x:[]
mergeSort xs = merge (mergeSort h1) (mergeSort h2)
  where
    [h1,h2] = halveList xs [[],[]] (div (length xs) 2)
halveList xs [h1, _] 0 = [h1, xs]
halveList (x:xs) [h1, h2] l = halveList xs [x:h1, h2] (l-1)

