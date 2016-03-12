sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ s [] = s
foldl' f s (x:xs) = foldl' f (f s x) xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' e xs = foldl (\s c -> if c == e then True else s) False xs
