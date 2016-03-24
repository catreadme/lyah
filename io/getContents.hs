import Data.Char

main = do
  stuff <- getContents
  putStr $ map toUpper stuff
