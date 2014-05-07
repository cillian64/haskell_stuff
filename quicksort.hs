-- Compactified version which always uses leftmost element as pivot
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs [i] = [i]
qs (p:l) = qs (left l p) ++ p : qs (right l p)
    where left :: (Ord a) => [a] -> a -> [a]
          left l p = [x | x <- l, x < p]
          
          right :: (Ord a) => [a] -> a -> [a]
          right l p = [x | x <- l, x >= p]

-- Uses a general pivot function which could be clever-ified
-- Also glomps all equal-to-pivot elements at each level (middle)
qs' :: (Ord a) => [a] -> [a]
qs' [] = []
qs' [i] = [i]
qs' l = qs' (left l (pivot l)) ++ middle l (pivot l) ++ qs' (right l (pivot l))
    where left :: (Ord a) => [a] -> a -> [a]
          left l p = [x | x <- l, x < p]

          middle :: (Eq a) => [a] -> a -> [a]
          middle l p = [x | x <- l, x == p]

          right :: (Ord a) => [a] -> a -> [a]
          right l p = [x | x <- l, x > p]

          pivot :: [a] -> a
          pivot l = head l -- for now!

