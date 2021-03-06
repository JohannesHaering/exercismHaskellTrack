updateCount '\00' y = y
updateCount 'A' [(a, ac), c, g, t] = [(a, ac + 1), c, g, t]
updateCount 'C' [a, (c, cc), g, t] = [a, (c, cc + 1), g, t]
updateCount 'G' [a, c, (g, gc), t] = [a, c, (g, gc + 1) , t]
updateCount 'T' [a, c, g, (t, tc)] = [a, c, g, (t, tc + 1)]



countNucleotides "" = [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
countNucleotides seq = [('A', a), ('C', c), ('G', g), ('T', t)]
    where a = length (filter (\x -> x == 'A') seq)
          c = length (filter (\x -> x == 'C') seq)
          g = length (filter (\x -> x == 'G') seq)
          t = length (filter (\x -> x == 'T') seq)

