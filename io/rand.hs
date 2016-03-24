import System.Random

main = do
  gen <- getStdGen
  putStrLn $  take 15 (randomRs ('a','z') gen)
