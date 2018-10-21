{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RenderDocument
    ( program
    )
where

import Control.Monad (filterM)
import Core.Program
import Core.Text
import Core.System
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HashMap
import System.FilePath.Posix (takeBaseName)
import System.Directory (doesFileExist)
import Text.Pandoc

readFragment :: FilePath -> Program None Pandoc
readFragment file = liftIO $ do
    contents <- T.readFile file
    result <- runIOorExplode (readMarkdown def contents)
    return result

produceResult :: String -> [Pandoc] -> Program None ()
produceResult name docs =
  let
    final = mconcat docs
  in liftIO $ do
    result <- runIOorExplode (writeLaTeX def final)
    T.writeFile (name ++ ".latex") result

usage :: String
usage = [quote|
Usage:

    publish <BookName.list>

where 'BookName' will be used as the base name for the intermediate
.latex file and the final output .pdf file. The list file should
contain filenames, one per line, of the fragments you wish to render
into a complete document.
|]

processBookFile :: FilePath -> Program None (String, [FilePath])
processBookFile file = liftIO $ do
    contents <- T.readFile file
    files <- filterM doesFileExist (possibilities contents)
    return (base, files)
  where
    base = takeBaseName file -- "/directory/file.ext" -> "file"

    -- filter out blank lines and lines commented out
    possibilities :: Text -> [FilePath]
    possibilities = map T.unpack . filter (not . T.null)
        . filter (not . T.isPrefixOf "#") . T.lines

-- this seems like it should be a standard utility function, lookupArgument
-- or something? Would prevent HashMap from leaking

extractBookFile :: Program None FilePath
extractBookFile = do
    params <- getCommandLine
    case lookupArgument "bookfile" params of
        Nothing -> invalid
        Just bookfile -> return bookfile

data UsageErrors
    = NoFileSpecified
    deriving Show

instance Exception UsageErrors

program :: Program None ()
program = do
    bookfile <- extractBookFile
    (name, files) <- processBookFile bookfile
    docs <- mapM readFragment files
    produceResult name docs

