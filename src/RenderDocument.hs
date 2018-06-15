{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

import Control.Monad (filterM)
import Data.String.Here
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit
import System.FilePath.Posix (takeBaseName)
import System.Directory (doesFileExist)
import Text.Pandoc

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
usage = putStrLn [here|
Usage:

    publish <BookName.list>"

where BookName will be used as the base name for the intermediate .latex file
and the final output .pdf file. The list file should contain filenames, one per
line, of the fragments you wish to render into a complete document.
|]

processBookFile :: [String] -> IO (String, [FilePath])
processBookFile [] = do
    putStrLn "Error: No book file specified"
    usage
    return ("",[])
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

    
