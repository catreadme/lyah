sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sum''' :: (Num a) => [a] -> a
sum''' xs = foldl (\s c -> s + c) 0 xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ s [] = s
foldl' f s (x:xs) = foldl' f (f s x) xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' e xs = foldl (\s c -> if c == e then True else s) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\c s -> f c : s) [] xs

sumsq :: (Integral a) => a -> a
sumsq n = foldr (\c s -> (c^2) + s) 0 [1..n]

sumsq' :: (Integral a) => a -> a
sumsq' 0 = 0
sumsq' n = (n^2) + sumsq' (n-1)

length' :: (Integral b) => [a] -> b
length' xs = foldr (\c s -> s + 1) 0 xs

length'' :: (Integral b) => [a] -> b
length'' xs = foldl (\s c -> s + 1) 0 xs

minList :: (Integral a) => [a] -> a
minList xs = foldr1 (\c s -> if s > c then s else c) xs

minList' :: (Integral a) => [a] -> a
minList' [] = error "Ooh boy.."
minList' [x] = x
minList' (x:xs)
  | x > minList' xs = x
  | otherwise = minList' xs

minList'' :: (Integral a) => [a] -> a
minList'' xs = foldl1 (\s c -> if s > c then s else c) xs

reverse' :: (Ord a) => [a] -> [a]
reverse' xs = foldr (\c s -> s ++ [c]) [] xs

reverse'' :: (Ord a) => [a] -> [a]
reverse'' xs = foldl (\s c -> c : s) [] xs

remove :: (Eq a) => [a] -> [a] -> [a]
remove xs ys = foldr (\c s -> if c `elem` xs then s else c : s) [] ys

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\c s -> if f c then c : s else s) [] xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = foldl (\s c -> if f c then s ++ [c] else s) [] xs

remdups :: (Eq a) => [a] -> [a]
remdups [] = []
remdups [x] = [x]
remdups (x:y:zs)
  | x == y = remdups (y:zs)
  | otherwise = x : remdups (y:zs)

maximum' :: (Ord a) => [a] -> a
maximum' xs = foldr1 (\c s -> if c > s then c else s) xs

product' :: (Num a) => [a] -> a
product' xs = foldl1 (\s c -> s*c) xs

head' :: [a] -> a
head' xs = foldr1 (\c _ -> c) xs

last' :: [a] -> a
last' xs = foldl1 (\_ c -> c) xs

adding :: (Num a) => [a] -> [a]
adding xs = scanl1 (\s c -> s + c) xs
