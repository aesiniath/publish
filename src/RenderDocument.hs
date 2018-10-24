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
import System.Posix.Temp (mkdtemp)
import Text.Pandoc

temporaryBuildDir :: Program None FilePath
temporaryBuildDir = do
    dirname <- liftIO $ mkdtemp "/tmp/publish-"
    debugS "tmpdir" dirname
    return dirname

readFragment :: FilePath -> Program None Pandoc
readFragment file = do
    debugS "fragment" file
    liftIO $ do
        contents <- T.readFile file
        result <- runIOorExplode (readMarkdown def contents)
        return result

produceResult :: FilePath -> String -> [Pandoc] -> Program None ()
produceResult tmpdir name docs =
  let
    final = mconcat docs
    target = tmpdir ++ "/" ++ name ++ ".latex"
  in do
    debugS "target" target
    liftIO $ do
        result <- runIOorExplode (writeLaTeX def final)
        T.writeFile target result

processBookFile :: FilePath -> Program None (String, [FilePath])
processBookFile file = do
    debugS "bookfile" file
    liftIO $ do
        contents <- T.readFile file
        files <- filterM doesFileExist (possibilities contents)
        return (base, files)
  where
    base = takeBaseName file -- "/directory/file.ext" -> "file"

    -- filter out blank lines and lines commented out
    possibilities :: Text -> [FilePath]
    possibilities = map T.unpack . filter (not . T.null)
        . filter (not . T.isPrefixOf "#") . T.lines

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
    event "Reading bookfile"
    (name, files) <- processBookFile bookfile

    event "Reading pieces"
    docs <- mapM readFragment files

    event "Write output"
    tmp <- temporaryBuildDir
    produceResult tmp name docs

    event "Complete"

