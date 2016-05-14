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

class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
  fail :: String -> m a
  fail s = error s

instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _ = []

-- next: The list monad
