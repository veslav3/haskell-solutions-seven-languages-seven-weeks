module Main where
  main = undefined

  tryIo = do putStr "Enter your name: " ;
             line <- getLine ;
             let { backwards = reverse line } ;
             return ("hello. Your name backwards is " ++ backwards)
