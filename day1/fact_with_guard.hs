module Main where
  main = undefined

  factorial :: Integer -> Integer
  factorial x | x > 1 = x * factorial (x - 1) | otherwise = 1
