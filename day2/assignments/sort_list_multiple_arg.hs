import Data.List

sortList :: (Ord a) => [a] -> (a -> a -> Ordering) -> [a]
sortList [] _ = []
sortList list function = sortBy function list
