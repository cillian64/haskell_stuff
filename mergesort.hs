mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort a = merge (mergesort left) (mergesort right)
    where
        (left, right) = splitAt (round $ (fromIntegral $ length a)/2 ) a
       
        merge :: (Ord a) => [a] -> [a] -> [a]
        merge [] [] = []
        merge a [] = a
        merge [] b = b
        merge (a:as) (b:bs)
            | a <= b = a : merge as (b:bs)
            | a >  b = b : merge bs (a:as)
