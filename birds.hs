type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l,r)
  | abs ((n + l) - r) < 4 = Just (n + l, r)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l,r)
  | abs ((n + r) - l) < 4 = Just (l,n + r)
  | otherwise = Nothing


pole = return (0,0) >>= landLeft 2 >>= landRight 3
-- Just (2,3)

poleFail = return (0,0) >>= landLeft 1 >>= landRight 1 >>= landLeft 10
-- Nothing

poleFail' = return (0,0) >>= landLeft 2 >> Nothing >>= landRight 3

-- adding some sugar

a = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

b = Just 3   >>= (\x ->
    Just "!" >>= (\y ->
    Just (show x ++ y)))

c = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

pole'' = do
  start <- return (0,0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second
