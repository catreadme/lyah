import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
  (fn1:fn2:_) <- getArgs
  cpFile fn1 fn2

cpFile :: FilePath -> FilePath -> IO ()
cpFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents
