module Main where
  main = undefined

  data Tree a = Children [Tree a] | Leaf a deriving (Show)
