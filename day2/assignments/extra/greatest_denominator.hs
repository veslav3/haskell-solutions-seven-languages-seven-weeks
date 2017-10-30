gcd2 :: (Integral a) => a -> a -> a
gcd2 x y = gcd2_ (abs x) (abs y)
  where
    gcd2_ a 0 = a
    gcd2_ a b = gcd2_ b (a `rem` b)
