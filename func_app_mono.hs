data CMaybe a = CNothing | CJust Int a
  deriving (Show, Eq)

instance Functor CMaybe where
  fmap f (CJust n x) = CJust (n + 1) (f x)
  fmap f CNothing = CNothing
-- (fmap id $ CJust 0 "foo") /= (id CJust 0 "foo")
-- q.e.d. in this form, CMaybe is not a valid functor

---------------------------------------------------------------

data Vielleicht a = Nichts | Einfach a
  deriving (Show, Eq)

instance Functor Vielleicht where
  fmap f (Einfach x) = Einfach (f x)
  fmap f Nichts = Nichts
-- (fmap id $ Einfach "foo") == (id Einfach "foo")
-- (fmap ((+2) . (*5)) (Einfach 2)) == (fmap (+2) (fmap (*5) (Einfach 2)))
-- q.e.d in this form, Vielleicht is a valid functor

instance Applicative Vielleicht where
  pure = Einfach
  Nichts <*> _ = Nichts
  (Einfach f) <*> x = fmap f x
-- (pure (+1) <*> Einfach 1) == (fmap (+1) $ Einfach 1)
