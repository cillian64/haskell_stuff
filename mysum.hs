recursiveSum :: (Num a) => [a] -> a
recursiveSum [] = 0
recursiveSum l = head l + recursiveSum (tail l)

vectorAdd:: (Num a) => (a,a) -> (a,a) -> (a,a)
vectorAdd (b1,b2) (c1,c2) = (b1+c1,b2+c2)

vectorSum :: (Num a) => [(a,a)] -> (a,a)
vectorSum [] = (0,0)
vectorSum l = head l `vectorAdd` vectorSum (tail l)

vectorToScalar :: (Num a) => (a,a) -> a
vectorToScalar (b,c) = b+c

