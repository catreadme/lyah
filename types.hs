data Point = Point Float Float
  deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = r^2*pi
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
