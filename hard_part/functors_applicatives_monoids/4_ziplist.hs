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
  often called a cartesian product, one has to create a new type of List
  in order to be able to defina a second, different applicative behaviour
  of lists.
-}
newtype ZipList a = ZipList {getList :: [a]}
  deriving (Show)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Functor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap f xs

instance Functor ZipList where
  fmap _ (ZipList []) = (ZipList [])
  fmap f (ZipList (x:xs)) = ZipList (f x : fmap f xs)

{-
  "(<$>)" is actually just "fmap" as an infix operator.
-}
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
