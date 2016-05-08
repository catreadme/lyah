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
               , String
               , Int
               , error
               , Maybe(Just, Nothing)
               , Either(Left, Right)
               , Applicative
               )

{-
  Monad class.
  "Take a fancy value, feed it into a function that takes a normal value and
  return a fancy value".
-}
class (Applicative m) => Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y

  fail :: String -> m a
  fail s = error s

{-
  Monad instance for Maybe.
  (>>=) can be viewed as a special form of function application.

  "Explicit" function application of a normal value into a function.
  ($) (+5) 5

  "Monadic" function application of a fancy value into a function.
  (>>=) (Just 5) (\x -> return (x+1))

-}
instance Monad Maybe where
  return x = Just x
  Nothing >>= _ = Nothing
  Just x >>= f = f x
  fail _ = Nothing

-- Examples:
ma = return 5 :: Maybe Int -- Just 5

mb = Just 5 >>= (\x -> Just (x+1)) -- Just 6
mc = Just 5 >>= (\x -> return (x+1)) -- Just 6

-- next:
--  do notation
