import Control.Monad

main = do
  line <- getLine
  when (line /= "") $ do
      putStrLn $ wordRev line
      main

wordRev :: String -> String
wordRev = unwords . map reverse . words
