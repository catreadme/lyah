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
               )
import Data.List

{-
  The Functor class again.
-}
class Functor f where
  fmap :: (a -> b) -> f a -> f b

{-
  This is how "Applicative" is defined in Prelude. The part with the "(*>)" and
  "(<*)" operators are omittet here for the sake of simplicity.

  "Take a function from "a" to "b" with a context "f", an "a" with some context,
  and return a "b" with that same context"
-}
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

{-
  The Maybe Functor again.
-}
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

{-
  Implementing the "Applicative" typeclass for the "Maybe" type.

  Maybe this implementation of "(<*>)" makes the nature of "Applicative" more
  obvious:
    (<*>) :: f (a -> b) -> f a -> f b
    (Just f) <*> (Just x) = Just (f x)
-}
instance Applicative Maybe where
  pure x = Just x
  Nothing <*> _ = Nothing
  (Just f) <*> x = fmap f x

{-
  "(<$>)" is actually just "fmap" as an infix operator.
-}
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

{-
  Implementing the "Applicative" typeclass for the "(->)" type.

  Since "Applicative" takes a type constructor of kind "* -> *", "->" has
  to be partially applied in order to be made an "Applicative". ("->" is of kind
  "* -> * -> *").
-}
instance Functor ((->) a) where
  fmap f g = (\x -> f (g x))

instance Applicative ((->) a) where
  pure x = (\_-> x)
  f <*> g = (\x -> f x (g x))

-- Next:
--  Another instance of Applicative is (->) r
