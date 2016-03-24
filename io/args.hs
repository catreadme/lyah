import System.Environment
import Data.List

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "Args:"
  mapM putStrLn args
  putStrLn "Prog:"
  putStrLn progName
