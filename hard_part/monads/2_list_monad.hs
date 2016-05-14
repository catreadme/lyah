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
               , (++)
               , (<$>)
               , (<*>)
               , (^)
               , (<)
               , show
               , concat
               , map
               , Show
               , String
               , Int
               , error
               , Maybe(Just, Nothing)
               , Either(Left, Right)
               , Applicative
               )
import Control.Monad(guard)


class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
  fail :: String -> m a
  fail s = error s

{-
  The list monad.
  Handle nondeterministic computations.
-}

instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _ = []

-- Examples:
la = [1,2,3] >>= (\x -> [(x,-x)]) -- [(1,-1),(2,-2),(3,-3)]
lb = [1,2] >>= (\x -> ['a', 'b'] >>= (\y -> return (x,y))) -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

lc = do
  x <- [1,2]
  y <- ['a','b']
  return (x,y)
  -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- List comprehensions are the List Monad.
ld = [(x, y) | x <- [1,2], y <- ['a','b']] -- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

le = [x | x <- [1..100], x < 10] -- [1,2,3,4,5,6,7,8,9]

lf = do
  x <- [1..100]
  guard (x < 10)
  return x
  -- [1,2,3,4,5,6,7,8,9]
