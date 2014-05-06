qs :: (Ord a) => [a] -> [a]
qs [] = []
qs [i] = [i]
qs l = qs (left l (pivot l)) ++ middle l (pivot l) ++ qs (right l (pivot l))

left :: (Ord a) => [a] -> a -> [a]
left l p = [x | x <- l, x < p]

middle :: (Eq a) => [a] -> a -> [a]
middle l p = [x | x <- l, x == p]

right :: (Ord a) => [a] -> a -> [a]
right l p = [x | x <- l, x > p]

pivot :: [a] -> a
pivot l = head l -- for now!

