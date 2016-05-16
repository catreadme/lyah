import Control.Monad.State
import System.Random

{-
  Stack without a Monad.
-}
type Stack = [Int]

pop' :: Stack -> (Int,Stack)
pop' (x:xs) = (x,xs)

push' :: Int -> Stack -> ((),Stack)
push' x s = ((), x:s)

st :: Stack
st = [1,2,3]

sb = pop' st -- (1,[2,3])
sc = push' 5 st -- ((),[5,1,2,3])

{-
  Stack with State Monad.
-}
pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((),x:xs)

sd :: State Stack Int
sd = do
  push 3
  pop
  pop

se = runState sd [1,2,3] -- (1,[2,3])

{-
  Randomness without a Monad.
-}
gen = mkStdGen 1
(randBool, newGen) = random gen :: (Bool, StdGen)
-- randBool = True
-- newGen = 80028 40692

-- True is Heads, False is Tails
tossThreeCoins :: StdGen -> (Bool, Bool, Bool)
tossThreeCoins gen =
  let (fstToss,newGen) = random gen
      (sndToss,newGen') = random newGen
      (trdToss,_) = random newGen'
  in  (fstToss,sndToss,trdToss)

coinTosses = tossThreeCoins $ mkStdGen 615 -- (True,False,False)

{-
  Randomness with State Monad.
-}
rs :: (RandomGen g, Random a) => State g a
rs = state random

tossThreeCoins' :: State StdGen (Bool,Bool,Bool)
tossThreeCoins' = do
  a <- rs
  b <- rs
  c <- rs
  return (a,b,c)

ra = runState tossThreeCoins' (mkStdGen 51)
