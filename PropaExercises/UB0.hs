max3_1 x y z = if x >= y && x >= z then x else if y >= x && y >= z then y else z
max3_2 x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | otherwise = z
max3_3 x y z = max x (max y z)
