getTriplets n = helper1 n 0
  where
    helper1 n x
      | x > n = []
      | otherwise = (helper2 n x 0) ++ (helper1 n (x+1))
    helper2 n x y
      | x + y > n = []
      | otherwise = [x, y, n - x - y] : (helper2 n x (y+1))

isPythagorianTriplet [x,y,z] = x ** 2 + y ** 2 == z **2 

getPythagorianTriplets n = filter isPythagorianTriplet (getTriplets n)
