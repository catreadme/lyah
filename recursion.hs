fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "oh boy.."
maximum' (x:[]) = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maximum' xs
  where maxTail = maximum' xs

replicate' :: (Integral a) => a -> b -> [b]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

take' :: (Integral a) => a -> [b] -> [b]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (x:xs) =
  let left = quick [a | a <- xs, a <= x]
      right = quick [a | a <- xs, a > x]
  in left ++ [x] ++ right
