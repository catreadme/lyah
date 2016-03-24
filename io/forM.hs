import Control.Monad

main = do
  colors <- forM [1..4] (\n -> do
    putStrLn $ "Enter color nr. " ++ show n
    color <- getLine
    return color)
  putStrLn "The colors are:"
  mapM putStrLn colors
