import Data.List
import Data.Char

numUniq :: (Eq b) => [b] -> Int
numUniq = length

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\s c -> if take nlen c == needle then True else s) False (tails haystack)

enc :: Int -> [Char] -> [Char]
enc _ [] = []
enc n (x:xs) = chr (n + (ord x)) : enc n xs

dec :: Int -> [Char] -> [Char]
dec _ [] = []
dec n (x:xs) = chr ((ord x) - n) : dec n xs
