import System.Environment
import System.IO
import System.IO.Error

main = do
  coutLines `catchIOError` errorHandler

coutLines :: IO ()
coutLines = do
  (fName:_) <- getArgs
  contents <- readFile fName
  putStrLn $ "The file has " ++  show (length (lines contents)) ++ " lines."

errorHandler :: IOError -> IO ()
errorHandler e
  | isDoesNotExistError e = putStrLn "Given file does not exist."
  | isPermissionError e = putStrLn "You do not have permissions to read this file."
  | otherwise = ioError e
