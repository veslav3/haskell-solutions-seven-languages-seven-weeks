everyThird :: Int -> [Int]
everyThird x = [x, (x + 3) ..]

everyFifth :: Int -> [Int]
everyFifth y = [y, (y + 5) ..]

everyEighth :: Int -> Int -> [Int]
everyEighth x y = (zipWith (+) (everyThird x) (everyFifth y))
