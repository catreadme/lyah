data Vector a = Vector a a a
  deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector x y z) (Vector x1 y1 z1) = Vector (x+x1) (y+y1) (z+z1)

addM :: (Num a) => Maybe a -> Maybe a -> Maybe a
addM _ Nothing = Nothing
addM Nothing _ = Nothing
addM (Just a) (Just b) = Just (a + b)
