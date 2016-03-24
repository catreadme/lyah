import System.IO

main = do
  handle <- openFile "file.hs" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
