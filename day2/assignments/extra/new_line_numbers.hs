module Main where

  changer :: [Char] -> [Char] -> [Char]
  changer [] res = res
  changer (x:xs) res = changer xs (res ++ (if x == ' ' then "\n" else if x == '.' then ".\n" else [x]))

  newLine :: [Char] -> [Char]
  newLine xs = changer xs ""

  main = print $ newLine "this is my test string which you can use to validate the output."

  renderLine :: Show a => Integer -> a -> String
  renderLine i a = show i ++ ". " ++ show a

  bs :: Show a => [a] -> String
  bs = unlines . zipWith renderLine [1..]
