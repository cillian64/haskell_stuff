qs :: (Ord a) => [a] -> [a]
qs [] = []
qs [i] = [i]
qs (p:l) = qs (left l p) ++ p : qs (right l p)

left :: (Ord a) => [a] -> a -> [a]
left l p = [x | x <- l, x < p]

right :: (Ord a) => [a] -> a -> [a]
right l p = [x | x <- l, x >= p]

