multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x*y*z

compWithHundred :: (Num a, Ord a) => a -> Ordering
compWithHundred = compare 100

divByTen :: (Floating a) => a -> a
divByTen = (/) 10

isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyMany :: (Integral a) => a -> (b -> b) -> b -> b
applyMany 0 _ x = x
applyMany 1 f x = f x
applyMany n f x = applyMany (n - 1) f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

quick' :: (Ord a) => [a] -> [a]
quick' [] = []
quick' (x:xs) =
  let left = quick' $ filter (<=x) xs
      right = quick' $ filter (>x) xs
  in left ++ [x] ++ right

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x = x : takeWhile' f xs
  | otherwise = []
