import Control.Monad (filterM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.FilePath.Posix -- Needed?
import System.Directory (doesFileExist)
import Text.Pandoc

checkFile :: FilePath -> IO Bool
checkFile file = doesFileExist file

readFragment :: FilePath -> IO Pandoc
readFragment file = do
    contents <- T.readFile file
    result <- runIOorExplode (readMarkdown def contents)
    return result

produceResult :: [Pandoc] -> IO ()
produceResult docs =
  let
    final = mconcat docs
  in do
    result <- runIOorExplode (writeLaTeX def final)
    T.writeFile "Junk.latex" result

main :: IO ()
main = do
    args <- getArgs
    files <- filterM checkFile args 
    docs <- mapM readFragment files
    produceResult docs

    
