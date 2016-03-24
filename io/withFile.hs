import System.IO

main = do
  withFile "withFile.hs" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)
