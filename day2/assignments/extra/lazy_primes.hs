primes :: [Integer]
primes = a [2..]
  where
    a (p:xs) = p : a [x | x <- xs, x `mod` p > 0]
