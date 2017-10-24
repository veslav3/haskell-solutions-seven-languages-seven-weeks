module Main where
  main = undefined

  allEven :: [Integer] -> [Integer]
  allEven [] = []
  allEven (h:t) = if even h then h:allEven t else allEven t

  allEven2 :: [Integer] -> [Integer]
  allEven2 [] = []
  allEven2 nums = [x | x <- nums, even x]
