import Data.Ratio
import Control.Monad
import Data.List

{-
  Representing how likely an occurance of a number is.
-}
la = [(3,1%2),(5,1%4),(6,1%4)]
-- 3 has a 50% chance of occurance
-- 5 has a 25% chance of occurance
-- 6 has a 25% chance of occurance

nestedProb :: Prob (Prob Char)
nestedProb = Prob[
  (Prob[
    ('a',1%2),('b',1%2)
  ], 1%4),
  (Prob[
    ('c',1%2),('d',1%2)
  ], 3%4)]

flatProb :: Prob (Prob a) -> Prob a
flatProb (Prob xs) = Prob $ concat $ map g xs
  where g (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

{-
  A Probability Type
-}

newtype Prob a = Prob {getProb :: [(a,Rational)]}
  deriving (Show)

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

instance Applicative Prob where
  pure x = Prob [(x,1%1)]
  _ <*> _ = undefined

instance Monad Prob where
  return = pure
  m >>= f = flatProb (fmap f m)
  fail _ = Prob []

{-
  Example
-}
data Coin = Heads | Tails
  deriving (Show, Eq)

coin :: Prob Coin
coin =  Prob [(Heads,1%2),(Tails,1%2)]

awesomeCoin :: Prob Coin
awesomeCoin = Prob [(Heads,1%10),(Heads,9%10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- awesomeCoin
  return (all (==Tails) [a,b,c])
