{-
  In order to cover the whole section from the ground up, there will be
  no Prelude to start with. Everything needed will be importet explicitly.
-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
  Those are the things needed from Preludede.
    - (+)
      To be able to write some examples.
    - Int
      To be able to write some examples.
    - Maybe
      To be able to implement "Applicative" for the "Maybe" type.
-}
import Prelude ( (+)
               , Int
               , Maybe(Nothing, Just)
               , Show
               , repeat
               , zipWith
               )
import Data.List

{-
  Since the default Applicative behaviour of Lists is to produce what is
  sometimes called a cartesian product, one has to create a new type of List
  in order to be able to defina a second, different applicative behaviour.
-}

{-
  A simple wrapper for a new type of List.
-}
newtype ZipList a = ZipList {getList :: [a]}
  deriving (Show)

{-
  Defining the Functor and Applicative classes.
-}
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

{-
  "(<$>)" is actually just "fmap" as an infix operator.
-}
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

{-
  Some preparation work.
-}
instance Functor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap f xs

instance Functor ZipList where
  fmap _ (ZipList []) = (ZipList [])
  fmap f (ZipList (x:xs)) = ZipList (f x : fmap f xs)

{-
  The ZipList Applicative.
-}
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- Examples:
za = fmap (+1) (ZipList [1,2,3]) -- ZipList {getList = [2,3,4]}
zb = (+1) <$> (ZipList [1,2,3]) -- ZipList {getList = [2,3,4]}

zc = ZipList [(+10), (+100)] <*> ZipList [1,2] -- ZipList {getList = [11,102]}
zd = (+) <$> ZipList [10,100] <*> ZipList [1,2] -- ZipList {getList = [11,102]}

{-
  Think of this like.
    (a -> b -> c) -> (f a -> f b -> f c)
-}
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- Examples:
la = liftA2 (+)
lb = la (ZipList [1,2]) (ZipList [10,20]) -- ZipList {getList = [11,22]}

lc = liftA2 (+) (ZipList [1,2]) (ZipList [10,20]) -- ZipList {getList = [11,22]}
