import qualified Data.Map as Map

data LockerState = Taken | Free
  deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lkey lmap = case Map.lookup lkey lmap of
  Nothing -> Left "Locker does not exist"
  Just (state, code) -> if state /= Taken
                        then Right code
                        else Left "Locker taken"

lockers :: LockerMap
lockers = Map.fromList [(0, (Free, "52-36"))
                       ,(1, (Taken, "59-21"))
                       ,(2, (Taken, "21-22"))
                       ,(3, (Free, "99-52"))
                       ]
