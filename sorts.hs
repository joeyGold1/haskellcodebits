isEmpty :: [Float] -> Bool
isEmpty a = (a == [])

greaterThanHead :: [Float] -> [Float]
greaterThanHead a = filter (>(head a)) a

equalToHead :: [Float] -> [Float]
equalToHead a = filter (==(head a)) a

lessThanHead :: [Float] -> [Float]
lessThanHead a = filter (<(head a)) a

quicksort :: [Float] -> [Float]
quicksort a = if (isEmpty a) then (a) else (quicksort (lessThanHead a) ++ equalToHead a ++ quicksort (greaterThanHead a))










flipthroughlist :: [Float] -> [Float]
flipthroughlist a = if (isEmpty (tail a)) then (a) else (if ((head a) <= (head (tail a))) then (head a):(flipthroughlist  (tail a)) else (flipthroughlist ((head (tail a)):(head a):(tail (tail a)))))

reverselist :: [Float] -> [Float]
reverselist a = if (isEmpty a) then (a) else (reverselist (tail a) ++ [head a])

nextsweep :: [Float] -> [Float]
nextsweep a = (bubblesort (reverse (tail (reverse a)))) ++ [head (reverse a)] 

bubblesort :: [Float] -> [Float]
bubblesort a = if (isEmpty a) then (a) else (nextsweep (flipthroughlist a))


