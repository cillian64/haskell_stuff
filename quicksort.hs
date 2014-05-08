-- Compactified version which always uses leftmost element as pivot
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs [i] = [i]
qs (p:l) = (qs left) ++ p : (qs right)
    where left = filter (<p) l
          right = filter (>=p) l

-- Uses a general pivot function which could be clever-ified
-- Also glomps all equal-to-pivot elements at each level (middle)
qs' :: (Ord a) => [a] -> [a]
qs' [] = []
qs' [i] = [i]
qs' l = (qs' left) ++ middle ++ (qs' right)
    where left = filter (<p) l
          middle = filter (==p) l
          right = filter (>p) l
          p = head l -- for now!

