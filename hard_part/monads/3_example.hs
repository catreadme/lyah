{-
  A knight's quest
  From the chapter "A fistful of Monads", Learn You a Haskell for Great Good

  "Here's a problem that really lends itself to being solved with
  non-determinism. Say you have a chess board and only one knight
  piece on it. We want to find out if the knight can reach a certain position
  in three moves. We'll just use a pair of numbers to represent the knight's
  position on the chess board. The first number will determine the column he's
  in and the second number will determine the row."
-}
import Control.Monad(guard)

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
             ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

reachableIn3 :: KnightPos -> [KnightPos]
reachableIn3 pos = return pos >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` reachableIn3 start
