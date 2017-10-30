module Main where
  main = undefined

  myRange start step = start:(myRange (start + step) step)
