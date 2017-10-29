module Main where
  main = undefined

  reverseList [] = []
  reverseList x = reverse x

  reverseList2 :: [a] -> [a]
  reverseList2 [] = []
  reverseList2 (x:xs) = (reverseList2 xs) ++ [x]
