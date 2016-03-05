doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

removeUpperCase :: [Char] -> [Char]
removeUpperCase xs = [x | x <- xs, x `elem` ['a'..'z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
