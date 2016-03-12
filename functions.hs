lucky :: (Integral a) => a -> String
lucky 7 = "lucky"
lucky x = "no lucky"

-- fact 3
{-
= 3 * (fact 2)
= 3 * (2 * fact 1)
= 3 * (2 * (1 * fact 0))
= 3 * (2 * (1 * 1))
-}
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

bad :: Char -> String
bad 'a' = "Alfons"
bad 'b' = "Bernard"

addVec :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVec x y = (fst x + fst y, snd x + snd y)

addVec' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVec' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

head' :: [a] -> a
head' [] = error "oh boy.."
head' (x:xs) = x

last' :: [a] -> a
last' [] = error "oh boy.."
last' (x:[]) = x
last' (x:xs) = last' xs

snd' :: [a] -> a
snd' [] = error "oh boy.."
snd' (x:[]) = error "oh boy.."
snd' (x:y:ys) = y

sndLast :: [a] -> a
sndLast [] = error "oh boy.."
sndLast (x:y:[]) = x
sndLast (y:ys) = sndLast ys

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

firstChar :: String -> String
firstChar "" = "Oh boy.."
firstChar input@(x:xs) = "The first char of " ++ input ++ " is " ++ [x]

bmi :: (RealFloat a) => a -> String
bmi x
  | x <= 18.5 = "underweight"
  | x <= 25.0 = "normal"
  | x <= 30.0 = "fat"
  | otherwise = "whale"

bmi' :: (RealFloat a) => a -> a -> String
bmi' w h
  | calc <= skinny = "underweight"
  | calc <= normal = "normal"
  | calc <= fat = "fat"
  | otherwise = "whale"
  where calc = w/h^2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' n m
  | n > m = n
  | otherwise = m

comp :: (Ord a) => a -> a -> Ordering
a `comp` b
  | a < b = LT
  | a == b = EQ
  | a > b = GT

initials :: String -> String -> String
initials fname lname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = fname
        (l:_) = lname

bmis :: (RealFloat a) => [(a, a)] -> [a]
bmis [] = []
bmis ((w,h):xs) = (calc w h) : bmis xs
  where calc w h = w/h^2

bmis' :: (RealFloat a) => [(a, a)] -> [a]
bmis' xs = [bmi w h | (w, h) <- xs]
  where bmi w h = w/h^2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2*r*pi*h
      topArea = r^2*pi
  in sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = case xs of [] -> error "oh boy.."
                       (x:_) -> x
