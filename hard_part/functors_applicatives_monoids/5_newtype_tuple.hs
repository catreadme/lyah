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
    - String
      To be able to write some examples.
    - Show
      To be able to write some examples.
-}
import Prelude ( (+)
               , Int
               , String
               , Show
               )

{-
  This section is probably the thing that I had the most trouble understanding
  in all of Haskell. In retrospect it seems trivial, it didn't at the time I
  first got in touch with it.
-}

{-
  Lets say we want a Functor for Tuples in such a way that
    fmap (+1) (1,1) returns (2,1)
  ..so it applies the function to the first element of the tuple, but not the
  second.
-}

{-
  Thi is how the Tuple is a Functor in Prelude. Note how the fact that it has
  to be partially applied in order to be made a Functor implies that the first
  element of the Tuple is of type "a" and the free one, let's call it "b"
  automaticaly is the second element, because "(,) a" is essentially
  "(a,)".
-}
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)

-- Examples:
ta = fmap (+5) (1,1) -- (1,6)

{-
  This is how to implement the fmap we want.
-}

newtype Pair a b = Pair (b, a)
  deriving (Show)

pa = Pair (1 :: Int, "" :: String) -- :: Pair String Int

instance Functor (Pair a) where
  fmap f (Pair (x,y)) = Pair (f x, y)

pb = fmap (+5) (Pair (1 :: Int,"" :: String)) -- Pair (6,1)

data Z a b = Z (b,a)
  deriving (Show)
