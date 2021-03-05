calculateNextValue x
    | mod x 2 == 0 = div x 2
    | otherwise = 3 * x + 1

calculateStepsHelper val steps
    | val == 1 = steps
    | otherwise = calculateStepsHelper (calculateNextValue val) (steps + 1)


calculateSteps x
    | x < 0 = -1
    | otherwise = calculateStepsHelper x 0 
