{-
  The Reader Monad is essentially the "(->) r" Monad.
  So there's really nothing special to it.
-}

-- Takes an input, applies (+10) to it, then (*2) to the input again, and adds
-- the two results together.
addThings :: Int -> Int
addThings = (+) <$> (*2) <*> (+10)

ab = addThings 5 -- (5+10) + (5*2) = 25

-- The Same as above just with a Monad.
addThings' :: Int -> Int
addThings' = do
  a <- (*2)
  b <- (+10)
  return (a+b)

ra = addThings' 5 -- 25
