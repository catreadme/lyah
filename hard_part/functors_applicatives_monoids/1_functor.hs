{-
  In order to cover the whole section from the ground up, there will be
  no Prelude to start with. Everything needed will be importet explicitly.
-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
  Those are the things needed from Preludede.
    - (+)
      To be able to write some examples.
    - Show
      To be able to display custom data types
    - Maybe(Just, Nothing)
      To be able to implement "Functor" for the "Maybe" type.
    - Either(Left, Right)
      To be able to implement "Functor" for the "Either" type.
-}
import Prelude ( (+)
               , Show
               , Maybe(Just, Nothing)
               , Either(Left, Right)
               )

{-
  This is how "Functor" is defined in Prelude. The part with the "(<$)" operator
  is omittet here for the sake of simplicity. From the type declaration it is
  alredy kind of obvious what this thing is supposed to do.

  "Take a function from "a" to "b", an "a" with some context, and return a "b" with
  that same context"
-}
class Functor f where
  fmap :: (a -> b) -> f a -> f b

{-
  Implementing the "Functor" typeclass for the "List" type.

  The implementation should look familiar. Here's how the well known "map"
  is defined in Prelude.
    map :: (a -> b) -> [a] -> [b]
    map _ []     = []
    map f (x:xs) = f x : map f xs
-}
instance Functor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap f xs

-- Examples:
la = fmap (+1) [1] -- [2]
lb = fmap (+1) [1..5] -- [2,3,4,5,6]

{-
  Implementing the "Functor" typeclass for the "Maybe" type.
-}
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

-- Examples:
ma = fmap (+1) (Just 1) -- Just 2
mb = fmap (+1) Nothing -- Nothing

{-
  Implementing the "Functor" typeclass for a "Tree" type.
-}

-- First the data type is declared.
data Tree a = Node a (Tree a) (Tree a) | Empty
  deriving (Show)

-- The actual "Functor" implementation for a "Tree" type.
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- Examples:
{-
       5
    3     8
  1  4  6  10
-}
tt = Node 5 (Node 3
              (Node 1 Empty Empty)
              (Node 4 Empty Empty))
            (Node 8
              (Node 6 Empty Empty)
              (Node 10 Empty Empty))

ta = fmap (+1) tt
 {-
    Node 6
      (Node 4
        (Node 2 Empty Empty)
        (Node 5 Empty Empty))
      (Node 9
        (Node 7 Empty Empty)
        (Node 11 Empty Empty))
 -}

 {-
   Implementing the "Functor" typeclass for the "Either" type.

   Since "Functor" takes a type constructor of kind "* -> *", Either has
   to be partially applied in order to be made a "Functor". (Either is of kind
   "* -> * -> *").
 -}

instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap _ (Left x) = Left x

-- Examples:
ea = fmap (+1) (Right 5) -- Right 6
eb = fmap (+1) (Left "Some error") -- Left "Some error"
