import Control.Monad (filterM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName)
import System.Directory (doesFileExist)
import Text.Pandoc

checkFile :: FilePath -> IO Bool
checkFile file = doesFileExist file

readFragment :: FilePath -> IO Pandoc
readFragment file = do
    contents <- T.readFile file
    result <- runIOorExplode (readMarkdown def contents)
    return result

produceResult :: String -> [Pandoc] -> IO ()
produceResult name docs =
  let
    final = mconcat docs
  in do
    result <- runIOorExplode (writeLaTeX def final)
    T.writeFile (name ++ ".latex") result

usage :: IO ()
usage = putStrLn "publish <BookName.list>"

processBookFile :: [String] -> IO (String, [FilePath])
processBookFile [] = usage >> error "No book file specified"
processBookFile (file:_) = do
    contents <- T.readFile file

    files <- filterM doesFileExist (possibilities contents)

    return (base, files)
  where
    base = takeBaseName file -- "/directory/file.ext" -> "file"

    -- filter out blank lines and lines commented out
    possibilities :: Text -> [FilePath]
    possibilities = map T.unpack . filter (not . T.null)
        . filter (not . T.isPrefixOf "#") . T.lines


main :: IO ()
main = do
    args <- getArgs
    (name, files) <- processBookFile args
    docs <- mapM readFragment files
    produceResult name docs

    
