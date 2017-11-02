retrieve :: Eq a => a -> [(a, b)] -> Maybe b
retrieve _ [] = Nothing
retrieve key ((k,v):rest) =
    if key == k
       then Just v
       else retrieve key rest

testData = [ ("autos", [("Audi",[("A5","A3")]), ("BMW",[("M3","M5")]), ("Seat",[("Altea","Leon")]) ] ),
             ("voedsel", [("Chips",[("Paprika","Joppie")]), ("Fruit",[("Appel","Peer")]) ] ) ]
