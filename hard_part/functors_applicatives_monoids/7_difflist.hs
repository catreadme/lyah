newtype DiffList a = DiffList ([a] -> [a])

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList ([]++)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f . g $ xs)

da = fromDiffList (toDiffList [1,2,3] `mappend` toDiffList [4,5,6])
-- [1,2,3,4,5,6]
