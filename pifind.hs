piFind :: (Integral a, Floating b) => a -> b
piFind numIts = sqrt (6 * iter numIts)

-- (iterate it) returns the sum after "it" iterations
-- Recurse down to iteration 0 which gives edge case sum=0
-- Then unwrap back out running the summation.
iter :: (Integral a, Fractional b) => a -> b
iter 0 = 0
iter it = 1.0 / fromIntegral (it * it) + iter (it - 1)
-- Each iteration returns the sum after it number of iterations

