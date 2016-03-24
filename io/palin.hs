main = do
  interact palindrome

palindrome :: String -> String
palindrome xs = unlines $ map (\xs -> if xs == reverse xs then "palindrome" else "no palindrome") (lines xs)
