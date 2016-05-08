{-
  In order to cover the whole section from the ground up, there will be
  no Prelude to start with. Everything needed will be importet explicitly.
-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
  Those are the things needed from Preludede.
-}
import Prelude ( (+)
               , (++)
               , (*)
               , (||)
               , (&&)
               , length
               , compare
               , Int
               , String
               , Bool(False, True)
               , Maybe(Nothing, Just)
               , Ordering(LT, EQ, GT)
               , Show
               , Num
               )

import Data.Foldable(foldr)

{-
  The Monoid class defines 3 functions.
-}
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

{-
  List Monoid.
  The "neutral" element of lists is "[]".
  The "append" behaviour of lists is "(++)"
-}
instance Monoid [a] where
  mempty = []
  mappend = (++)

{-
  There are multiple ways for Numbers to be monoids. One way is to behave as a
  Sum.
-}
newtype Sum a = Sum a
  deriving (Show)

{-
  There are multiple ways for Numbers to be monoids. One way is to behave as a
  Product.
-}
newtype Product a = Product a
  deriving (Show)

{-
  The "neutral" element of addition is "0".
  The "append" behaviour of addition is "(+)"
-}
instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x+y)

{-
  The "neutral" element of multiplication is "1".
  The "append" behaviour of multiplication is "(*)"
-}
instance (Num a) => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x*y)

{-
  Turns out Bools can also be Monoids in different ways.
-}

{-
  There are multiple ways for Bools to be monoids. One way is to behave as
  Any.
-}
newtype Any = Any Bool
  deriving (Show)

{-
  There are multiple ways for Bools to be monoids. One way is to behave as
  All.
-}
newtype All = All Bool
  deriving (Show)

{-
  The "neutral" element of Any is "False".
  The "append" behaviour of Any is "(||)"
-}
instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x||y)

{-
  The "neutral" element of All is "False".
  The "append" behaviour of All is "(&&)"
-}
instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x&&y)

{-
  Ordering is also a Monoid.
-}
instance Monoid Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT

--Example:

{-
  Compares two Strings, first by length and if they are equal then by
  the alphabetical order of the Characters.
-}
comp :: String -> String -> Ordering
comp x y = (length x `compare` length y) `mappend`
           (x `compare` y)

{-
 Maybe can act in many ways as a Monoid.
-}

{-
  If the Type parameter of Maybe is also a Monoid, the Maybe Monoid can
  just use it's (the type parameter's) Monoid class.
-}
instance (Monoid m) => Monoid (Maybe m) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just x `mappend` Just y = Just (x `mappend` y)

{-
  Another way for Maybe to be a Monoid is to simply take the First element and
  discard the second one.
-}
newtype First a = First (Maybe a)
  deriving (Show)

instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First (Just x)
  First Nothing `mappend` x = x

{-
  Another way for Maybe to be a Monoid is to simply take the Last element and
  discard the first one.
-}
newtype Last a = Last (Maybe a)
  deriving (Show)

instance Monoid (Last a) where
  mempty = Last Nothing
  _ `mappend` Last (Just x) = Last (Just x)
  x `mappend` Last Nothing = x

-- TODO Foldable and foldMap

{-
  Reminder of how foldl works.
-}
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ s [] = s
foldl f s (x:xs) = foldl f (f s x) xs
{-
  foldl (+) 0 [1,2,3]
    foldl (+) ((+) 0 1) [2,3]
      foldl (+) ((+) ((+) 0 1) 2) [3]
        foldl (+) ((+) ((+) ((+) 0 1) 2) 3) []
          And since "foldl _ s [] = s" we return s which is:
            ((+) ((+) ((+) 0 1) 2) 3)
-}
