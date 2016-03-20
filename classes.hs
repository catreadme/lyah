data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
  show Red = "It is Red"
  show Yellow = "It is Yellow"
  show Green = "It is Green"

data Vielleicht a = Nichts | Einfach a
  deriving (Show)

instance (Eq a) => Eq (Vielleicht a) where
  Einfach x == Einfach y = x == y
  Nichts == Nichts = True
  _ == _ = False

instance Functor Vielleicht where
  fmap f (Einfach x) = Einfach (f x)
  fmap f Nichts = Nichts
