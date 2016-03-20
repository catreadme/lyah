import qualified Data.Map as Map

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Show, Read, Eq)

data Day = Monday | Tuseday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Read, Enum, Bounded, Eq, Ord)

type Name = String
type Number = String
type PhoneBook = [(Name, Number)]
type IntMap v = Map.Map Int v
