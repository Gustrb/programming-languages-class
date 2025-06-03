-- implement merge sort

-- Split a list into two halves
split :: [a] -> ([a], [a])
split xs = splitAt (length xs `div` 2) xs

-- Merge two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Merge sort implementation
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where (left, right) = split xs

-- Main function to demonstrate merge sort
main :: IO ()
main = do
    let unsortedList = [5, 2, 9, 1, 7, 6, 3, 8, 4]
    let sortedList = mergeSort unsortedList
    putStrLn "Original list:"
    print unsortedList
    putStrLn "\nSorted list:"
    print sortedList