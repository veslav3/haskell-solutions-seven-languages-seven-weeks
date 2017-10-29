module Main where
  main = undefined

  allEven :: [Integer] -> [Integer]
  allEven [] = []
  allEven (h:t) = if even h then h:allEven t else allEven t

  allEven2 :: [Integer] -> [Integer]
  allEven2 [] = []
  allEven2 nums = [x | x <- nums, even x]

  allEven3 :: [Integer] -> [Integer]
  allEven3 [] = []
  allEven3 nums = filter even nums

  allEven4 :: [Integer] -> [Integer]
  allEven4 [] = []
  allEven4 nums = [x | x <- nums, mod x 2 == 0]
