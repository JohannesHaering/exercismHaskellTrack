getSaddlePoints [] = [] 
getSaddlePoints (xs:[]) = []
getSaddlePoints (xs:ys:[]) = [] 
getSaddlePoints ls = helper ls 1
  where
    helper [] _ = []
    helper (xs:[]) _ = []
    helper (xs:ys:[]) _ = []
    helper (xs:ys:zs:rs) row = (helperRowBegin xs ys zs [row , 0]) ++ (helper (ys:zs:rs)  (row + 1))
    helperRowBegin [] _ _ _ = []
    helperRowBegin (x1:[]) _ _ _ = []
    helperRowBegin (x1:x2:xs) (y1:y2:ys) (z1:z2:zs) [p1 , p2]
      | y1 >= y2 && y1 <= x1 && y1 <= z1 = [p1, p2] : (helperRow (x1:x2:xs) (y1:y2:ys) (z1:z2:zs) [p1, p2 + 1])
      | otherwise = helperRow (x1:x2:xs) (y1:y2:ys) (z1:z2:zs) [p1, p2 + 1]
    helperRow [] _ _ _ = []
    helperRow (x1:[]) _ _ _ = []
    helperRow (x1:x2:[]) (y1:y2:[]) (z1:z2:[]) [p1, p2]
      | y2 >= y1 && y2 <= x2 && y2 <= z2 = [p1, p2] : []
      | otherwise = []
    helperRow (x1:x2:x3:xs) (y1:y2:y3:ys) (z1:z2:z3:zs) [p1, p2]
      | y2 >= y1 && y2 >= y3 && y2 <= x2 && y2 <= z2 = [p1, p2] : (helperRow (x2:x3:xs) (y2:y3:ys) (z2:z3:zs) [p1, p2 + 1])
      | otherwise = helperRow (x2:x3:xs) (y2:y3:ys) (z2:z3:zs) [p1, p2 + 1]

