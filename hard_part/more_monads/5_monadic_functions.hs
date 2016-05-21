import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

{-
  liftM
  (fmap, <$>, liftA)
-}
liftM' :: Monad m => (a1 -> r) -> m a1 -> m r
liftM' f m = m >>= (\x -> return $ f x)

la = liftM' (+5) (Just 5) -- Just 10
lb = liftM2 (+) (Just 5) (Just 5) -- Just 10
lc = liftM3 (\x y z -> x+y+z) (Just 5) (Just 5) (Just 5) -- Just 15
ld = runWriter $ liftM (+10) $ writer (0,"test")

{-
  ap
  (<*>)
-}
ap' :: Monad m => m (a -> b) -> m a -> m b
mf `ap'` m = do
  f <- mf
  x <- m
  return (f x)

aa = Just (+5) `ap'` Just 2 -- Just 7

{-
  join
  m >>= f == join (fmap f m)
-}
join' :: (Monad m) => m (m a) -> m a
join' m = m >>= (\x -> x)

ja = join' (Just (Just 5)) -- Just 5
jb = join [[1],[2]] -- [1,2]
jc = runWriter $ join' (writer (writer (1,"test"), "test")) -- (1,"testtest")
jd = join (Right (Right 5)) -- Right 5
je = join (Left "error") -- Left "error"

{-
  filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
-}
keepOdds :: Int -> Writer [String] Bool
keepOdds n
  | odd n = do
    tell [show n ++ " is odd, keeping it"]
    return True
  | otherwise = do
    tell [show n ++ " is even, throwing it away"]
    return False

ka = fst $ runWriter $ filterM keepOdds [0..5] -- [1,3,5]

kb = mapM_ putStrLn $ snd $ runWriter $ filterM keepOdds [0..5]
-- 0 is even, throwing it away
-- 1 is odd, keeping it
-- 2 is even, throwing it away
-- 3 is odd, keeping it
-- 4 is even, throwing it away
-- 5 is odd, keeping it

{-
  TODO I still don't really get this
  powerset :: [a] -> [[a]]
-}
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

pa = mapM_ putStrLn (map show $ powerset [1,2,3])
-- [1,2,3]
-- [1,2]
-- [1,3]
-- [1]
-- [2,3]
-- [2]
-- [3]
-- []

{-
  foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
-}
addSmall :: Int -> Int -> Maybe Int
addSmall x y
  | y > 9 = Nothing
  | otherwise = Just (x + y)

ada = addSmall 5 2 -- Just 7
adb = addSmall 10 5 -- Nothing

afa = foldM addSmall 0 [1,2,3] -- Just 6
afb = foldM addSmall 0 [1,20] -- Nothing
