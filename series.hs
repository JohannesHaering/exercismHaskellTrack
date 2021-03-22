getSeries series n = getSeriesAcc series n (initAccs n) []

getSeriesAcc [] n accs finished = finished
getSeriesAcc (x:xs) n accs finished = getSeriesAcc xs n (updateAccs accs x "") (updateFinished accs finished n)

initAccs 0 = []
initAccs n = [] : (initAccs (n-1))

updateFinished (x:[]) finished n
  | (length x) < n = finished
  | otherwise =  x : finished
updateFinished (x:xs) finished n = updateFinished xs finished n

updateAccs :: [[a]] -> a -> [a] -> [[a]]
updateAccs (x:[]) y last = [(last ++ [y])]
updateAccs (x:xs) y last = (last ++ [y]) : (updateAccs xs y x)
