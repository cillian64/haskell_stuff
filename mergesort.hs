mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort a = merge (mergesort (split_odd a)) (mergesort (split_even a))
    where
        split_odd :: [a] -> [a] -- Take just the odd elements
        split_odd [] = []
        split_odd [a] = []
        split_odd (_:a:as) = a : split_odd as
        
        split_even :: [a] -> [a] -- Take just the even elements
        split_even [] = []
        split_even [a] = [a]
        split_even (a:_:as) = a : split_even as
        
        merge :: (Ord a) => [a] -> [a] -> [a]
        merge [] [] = []
        merge a [] = a
        merge [] b = b
        merge (a:as) (b:bs)
            | a <= b = a : merge as (b:bs)
            | a >  b = b : merge bs (a:as)
