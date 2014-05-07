-- (iterate it) returns the sum after "it" iterations
-- Recurse down to iteration 0 which gives edge case and starting point, sum=0
-- Then unwrap back out running the summation.

piFind :: (Integral a, Floating b) => a -> b
piFind numIts = sqrt (6 * iter numIts)
    where
          iter :: (Integral a, Fractional b) => a -> b
          iter 0 = 0
          iter it = 1.0 / fromIntegral (it * it) + iter (it - 1)




-- Reverse recursion:
-- If we want to run until a specified precision, we can't use the above form
-- where the deepest recursion is the edge case and starting point.
--
-- Instead, the shallowest level is the starting point.  We recurse deeper
-- until the specified precision is reached.

piFind' :: (Floating a, Ord a) => a -> a
piFind' precision = piFromSum (recurse 0 1)
    where
          piFromSum sum = sqrt(6 * sum)

          recurse sum level
              | error < precision/2 = sum
              | otherwise = recurse newsum (level+1)
              where
                  newsum = sum + 1 / fromIntegral (level^2)
                  error = sqrt((pi - (piFromSum sum))^2)

