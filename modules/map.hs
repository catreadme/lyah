import qualified Data.Map as Map
import qualified Data.Set as Set

phoneBook =
  [("andy","555-236")
  ,("eva","628-121")
  ,("bob","222-222")
  ]

findByKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findByKey _ [] = Nothing
findByKey k ((key,v):xs)
  | k == key = Just v
  | otherwise = findByKey k xs

findByKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findByKey' k xs = foldl (\s (key, value) -> if k == key then Just value else s) Nothing xs

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' xs = foldr (\(key, value) s -> Map.insert key value s) Map.empty xs
