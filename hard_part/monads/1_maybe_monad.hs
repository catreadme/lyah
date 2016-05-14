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

-- A computation that might fail.
kickLastElement :: [a] -> Maybe [a]
kickLastElement [] = Nothing
kickLastElement (x:[]) = Just []
kickLastElement (x:xs) = (:) <$> Just x <*> Just xs >>= kickLastElement

-- Examples:
ka = kickLastElement [1,2] -- Just [1]
kb = kickLastElement [1] -- Just []
kc = kickLastElement [] -- Nothing

kd = Just [1,2] >>= kickLastElement -- Just [1]
ke = return [1,2] >>= kickLastElement -- Just [1]

kf = return [1,2,3,4] >>= kickLastElement >>= kickLastElement >>= kickLastElement -- Just [1]
kg = return [1,2] >>= kickLastElement >>= kickLastElement -- Just []
kh = return [1,2] >>= kickLastElement >>= kickLastElement >>= kickLastElement -- Nothing

-- Make the computation fail intentionally:
ki = return [1,2,3] >>= kickLastElement >> Nothing >>= kickLastElement -- Nothing

{-
  do Notation
-}
da = Just 5 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

db = Just 5   >>= (\x ->
     Just "!" >>= (\y ->
     Just (show x ++ y)))

dc = do
  x <- Just 5
  y <- Just "!"
  Just (show x ++ y)

dd = do
  x <- Just 5
  y <- Just "!"
  return (show x ++ y)

-- Example: kickLastElement
de = return [1,2] >>= kickLastElement >>= kickLastElement -- Just []

df = do
  list <- return [1,2]
  kickOne <- kickLastElement list
  kickLastElement kickOne
  -- Just []

-- Example: pattern matching
dg = do
  (x:xs) <- Just "test"
  return x
  -- Just 't'

-- When pattern matching fails inside do notation, "fail" is called.
dh = do
  (x:[]) <- Just "test"
  return x
  -- Nothing
