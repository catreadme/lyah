import System.Environment
import System.Directory
import System.IO
import Data.List

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add)
           ,("view", view)
           ,("remove", remove)
           ]

add :: [String] -> IO ()
add [fileName, todoItem] = do
  appendFile fileName (todoItem ++ "\n")
add _ = do

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n t -> show n ++ " - " ++ t) [1..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  (tempName, tempHandle) <- openTempFile "." "temp"
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
