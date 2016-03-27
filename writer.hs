import Data.Monoid
import Control.Monad.Writer

isBigGang :: Int -> (Bool,String)
isBigGang x = (x > 9,"Compared gang size to 9.")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f =
  let (y,newLog) = f x
  in (y,log `mappend` newLog)

-------------------------------------------

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-------------------------------------------

logNum :: Int -> Writer [String] Int
logNum x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNum 3
  b <- logNum 5
  tell ["Gonna do some multiplication"]
  return (a*b)

-------------------------------------------

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)
