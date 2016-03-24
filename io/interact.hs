import Data.Char

main = do
  interact shortLines

shortLines :: String -> String
shortLines xs =
  let allLines = lines xs
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result

-- oneliner
-- main = interact $ unlines . filter ((<10) . length) .lines
