addOne :: String -> Int
addOne x = (read x) + 1

stringify :: Int -> String
stringify x = show x

stringAdder :: String -> String
stringAdder = stringify . addOne
