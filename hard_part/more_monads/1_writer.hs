import qualified Data.Monoid as M

type Log = String

isBig :: Int -> (Bool,Log)
isBig x = (x > 9,"Compared to 9.")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,l) f = let (y,nl) = f x in (y,l `mappend` nl)

aa = (50,"Huge.") `applyLog` isBig -- (True,"Huge.Compared to 9.")
ab = ("test","Short.") `applyLog` (\x -> (length x, "Checked the length.")) -- (4,"Short.Checked the length.")
ac = ("milk",M.Sum 5) `applyLog` (\x -> (x ++ " and bread",M.Sum 7)) -- ("milk and bread",Sum {getSum = 12})

newtype Writer w a = Writer (a,w)
  deriving (Show, Read, Eq)

instance (Monoid w) => Functor (Writer w) where
  fmap f (Writer (x,l)) = Writer (f x,l)

instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer (x,mempty)
  Writer (f,l) <*> x = fmap f x

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x,l)) >>= f = let (Writer (y,nl)) = f x in Writer (y,l `mappend` nl)

-- Example 1:
doubleNum :: Int -> Writer [String] Int
doubleNum x = Writer (x*2, ["doubled number " ++ show x])

multNum :: Int -> Int -> Writer [String] Int
multNum x y = Writer (x*y, ["multiplied " ++ show x ++ " with " ++ show y])

wa = Writer (True, "") -- Writer (True,"") :: Writer String Bool

wb = return 5 >>= doubleNum -- Writer (10,["doubled number 5"])
wc = return 5 >>= doubleNum >>= doubleNum -- Writer (20,["doubled number 5","doubled number 10"])

wd = do
  x <- doubleNum 5 -- 10
  y <- doubleNum 10 -- 20
  multNum x y -- 200
  -- Writer (200,["doubled number 5","doubled number 10","multiplied 10 with 20"])

-- Example 2:
type Amount = Int

buyMilk :: Amount -> Writer (M.Sum Int) String
buyMilk n = Writer (show n ++ " milk", M.Sum $ n*5)

buyBeer :: Amount -> Writer (M.Sum Int) String
buyBeer n = Writer (show n ++ " beer", M.Sum $ n*8)

we = do
  x <- buyMilk 2 -- "2 milk"
  y <- buyBeer 3 -- "3 beer"
  return (x ++ " and " ++ y)
  -- Writer ("2 milk and 3 beer",Sum {getSum = 34})
