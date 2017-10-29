module Main where
  main = undefined

  colors = ["black", "white", "blue", "yellow", "red"]
  twoTuples = [(a, b) | a <- colors, b <- colors, a /= b]
