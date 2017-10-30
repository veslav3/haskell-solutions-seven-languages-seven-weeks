module Main where
  main = undefined

  colors = ["red", "green", "blue"]
  states = ["Alabama", "Mississippi", "Georgia", "Tennessee", "Florida"]
  adjacentStates = [("Tennessee", "Mississippi", "Georgia", "Alabama"),
    ("Mississippi", "Tennessee", "Alabama"),
    ("Alabama", "Mississippi", "Georgia", "Tennessee"),
    ("Georgia", "Tennessee", "Alabama", "Florida"),
    ("Florida", "Georgia", "Alabama")]
  combined = [(x, y) | x <- states, y <- colors]


  mapColoring = [ ("Tennesee", t, "Mississippi", m, "Alabama", a, "Georgia", g, "Florida", f) |
                  t <- colors, m <- colors, a <- colors, g <- colors, f <- colors,
                  m /= t, m /= a,
                  a /= t, a /= g, a /= f,
                  g /= f, g /= t ]
