stringToNumber :: String -> Float
stringToNumber value = read strippedValue :: Float where
  strippedValue = foldl (\
    newString c ->
      if (c == '$' || c == ',')
        then newString
        else newString ++ [c]
    ) "" value
