module Main where
  import Data.Char (toUpper)
  import Data.List (intercalate)
  import Data.Maybe (isNothing, fromJust)
  import System.IO (hFlush, stdout)

  main :: IO ()
  main = do
    putStrLn "Starting game..."
    putStrLn "Type quit to exit the game."
    let newBoard = empty 3
      in do (putStrLn . (\s->"\n"++s++"\n") . printBoard) newBoard
            gameExecution Nothing newBoard

  data Move        = O | X
                       deriving (Eq, Show, Enum, Ord)
  type Position    = (Char, Int)
  data BoardMove   = BoardMove
                     { bMove :: Maybe Move, bPos :: Position }
                       deriving (Eq, Show)
  type Board       = [BoardMove]
  type InvalidMove = String

  coord = (['A'..], [1..])

  empty :: Int -> Board
  empty size = do
    x <- take size (fst coord)
    y <- take size (snd coord)
    return $ BoardMove Nothing (x,y)

  printBoard :: Board -> String
  printBoard b = intercalate "\n" $
                   map (\row-> [(fst . bPos) (row !! 0)] ++ "]   | " ++
                               (intercalate " | "
                                  $ map (\bm-> maybe " " show $ bMove bm) row)
                               ++ " |")
                   (cut 3 b)

  cut :: Int -> [a] -> [[a]]
  cut n [] =  []
  cut n xs =  take n xs : cut n (drop n xs)

  gameExecution prevMove board = do
    let currPlayer = maybe X (\(BoardMove mv _) ->
                                 case mv of
                                   Just X -> O
                                   Just O -> X) prevMove
    putStr $ "Player '" ++ (show currPlayer) ++ "': "
    hFlush stdout
    playerMove <- getLine
    case (playerMove, (map toUpper playerMove) `elem` allCoord) of
      ("quit", _) ->
          putStrLn "Thanks for playing, come again!"
      (_, False)  -> do
          putStrLn $ "Possible options: " ++ intercalate ", " allCoord
          gameExecution prevMove board
      otherwise   -> do
          let pos = (toUpper $ playerMove !! 0,
                     read [(playerMove !! 1)] :: Int)
              currMove = BoardMove (Just currPlayer) pos
              currBoard = move currMove board
          either putStrLn (putStrLn . (\s->"\n"++s++"\n") . printBoard) currBoard
          case currBoard of
            Right r  -> if win currMove r
                          then do putStrLn $ "Player '"
                                             ++ (show currPlayer) ++"' wins!"
                                  main
                          else if draw currMove r
                                  then do putStrLn $ "It's a draw!"
                                          main
                                  else gameExecution (Just currMove) r
            Left err -> gameExecution prevMove board
    where allCoord = [[x] ++ show y | x <- take 3 (fst coord),
                                      y <- take 3 (snd coord)]

  move :: BoardMove -> Board -> Either InvalidMove Board
  move (BoardMove _ (c,r)) [] =
    Left $ "Could not make the move to given position " ++ [c] ++ (show r)
  move bm@(BoardMove nmov npos) (x:xs)
    | findMove x = Right $ bm:xs
    | otherwise  =
      case move bm xs of
        Right r -> Right $ x:r
        err     -> err
    where findMove (BoardMove m p) =
            p == npos && isNothing m && nmov /= Nothing

  draw :: BoardMove -> Board -> Bool
  draw bm b = not (any (isNothing . bMove) b)
           && not (win bm b)

  win :: BoardMove -> Board -> Bool
  win (BoardMove Nothing _) _ = False
  win (BoardMove m (c,r)) b = row || col || diag' cb || diag' (reverse cb)
   where row = length
               (filter (\(BoardMove m2 (_,r2)) ->
                         m2 == m && r2 == r) b) == 3
         col = length
               (filter (\(BoardMove m2 (c2,_)) ->
                         m2 == m && c2 == c) b) == 3
         diag' xss = all (\(BoardMove m2 _) ->
                           m2 == m) $ diag xss
         cb = cut 3 b

  diag :: [[a]] -> [a]
  diag xss = [xss !! n !! n | n <- [0 .. length xss - 1]]
