
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort x = (quicksort (lessthanfirst x)) ++ (equaltofirst x) ++ (quicksort (greaterthanfirst x))

lessthanfirst :: [Int] -> [Int]
lessthanfirst x = filter (<(head x)) x

greaterthanfirst :: [Int] -> [Int]
greaterthanfirst x = filter (>(head x)) x

equaltofirst :: [Int] -> [Int]
equaltofirst x = filter (==(head x)) x

isEmpty :: [Int] -> Bool
isEmpty x = x == []
