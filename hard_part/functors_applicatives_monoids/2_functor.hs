{-
  In order to cover the whole section from the ground up, there will be
  no Prelude to start with. Everything needed will be importet explicitly.
-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
  Those are the things needed from Preludede.
    - (++)
      To be able to write some examples.
    - (+)
      To be able to write some examples.
    - IO
      To be able to implement "Functor" for the "IO" type.
    - return
      To be able to implement "Functor" for the "IO" type.
    - getLine
      To be able to implement "Functor" for the "IO" type.
-}
import Prelude ( (++)
               , (+)
               , Show
               , IO
               , return
               , getLine
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
  Implementing the "Functor" typeclass for the "IO" type.
-}
instance Functor IO where
  fmap f ioaction = do
    ioresult <- ioaction
    return (f ioresult)

-- Examples:
ia = fmap (++ "!") getLine -- "(whatever getLine returns)!"

{-
  Implementing the "Functor" typeclass for the "->" type.
  Turns out that one ends up with function composition.

  The implementation should look familiar. Here's how the well known "."
  is defined in Prelude.
    (.) :: (b -> c) -> (a -> b) -> a -> c
    (.) f g = (\x -> f (g x))

  Since "Functor" takes a type constructor of kind "* -> *", "->" has
  to be partially applied in order to be made a "Functor". ("->" is of kind
  "* -> * -> *").

  "fmap of a function "f" over a function "g" returns a function lambda that
  first applies "g" to "x" and then "f" to that result"
-}
instance Functor ((->) a) where
  fmap f g = (\x -> f (g x))

-- Examples:
aa = fmap (++"!") (++"?") -- (++"!") . (++"?")
ab = aa "wait, what" -- "wait, what?!"

{-
  So since "fmap :: (a -> b) -> f a -> f b" one can think of how this would
  look like if it was curried. Turns out one gets
  "fmap :: (a -> b) -> (f a -> f b)". fmap takes an (a -> b) function and turns
  it into an (f a -> f b) function. Awesome. This is called "lifting" a function.

  (+2) :: a -> a
  fmap (+2) :: f b -> f b
-}
