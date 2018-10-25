{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RenderDocument
    ( program
    , initial
    )
where

import Control.Monad (filterM)
import Core.Program
import Core.System
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HashMap
import System.FilePath.Posix (takeBaseName)
import System.Directory (doesFileExist)
import System.Posix.Temp (mkdtemp)
import System.Process.Typed (proc, runProcess_, setStdin, closed)
import Text.Pandoc

data Env = Env
    { targetFileFrom :: FilePath
    , outputFileFrom :: FilePath
    , tempDirectoryFrom :: FilePath
    }

initial = Env "" "" ""

program :: Program Env ()
program = do
    bookfile <- extractBookFile
    populateEnvironment bookfile

    event "Reading bookfile"
    files <- processBookFile bookfile

    event "Reading pieces"
    docs <- mapM readFragment files

    event "Write intermediate"
    produceResult docs

    event "Render document"
    renderPDF

    event "Complete"

extractBookFile :: Program Env FilePath
extractBookFile = do
    params <- getCommandLine
    case lookupArgument "bookfile" params of
        Nothing -> invalid
        Just bookfile -> return bookfile

populateEnvironment :: FilePath -> Program Env ()
populateEnvironment file = do
    tmpdir <- temporaryBuildDir

    let env = Env
            { targetFileFrom = tmpdir ++ "/" ++ base ++ ".latex"
            , outputFileFrom = tmpdir ++ "/" ++ base ++ ".pdf"
            , tempDirectoryFrom = tmpdir
            }

    setApplicationState env
  where
    base = takeBaseName file -- "/directory/file.ext" -> "file"

processBookFile :: FilePath -> Program Env [FilePath]
processBookFile file = do
    debugS "bookfile" file
    files <- liftIO $ do
        contents <- T.readFile file
        filterM doesFileExist (possibilities contents)

    return files
  where
    -- filter out blank lines and lines commented out
    possibilities :: Text -> [FilePath]
    possibilities = map T.unpack . filter (not . T.null)
        . filter (not . T.isPrefixOf "#") . T.lines

readFragment :: FilePath -> Program Env Pandoc
readFragment file = do
    debugS "fragment" file
    liftIO $ do
        contents <- T.readFile file
        result <- runIOorExplode (readMarkdown def contents)
        return result

temporaryBuildDir :: Program Env FilePath
temporaryBuildDir = do
    dirname <- liftIO $ mkdtemp "/tmp/publish-"
    debugS "tmpdir" dirname
    return dirname

produceResult :: [Pandoc] -> Program Env ()
produceResult docs =
  let
    final = mconcat docs
  in do
    env <- getApplicationState
    let target = targetFileFrom env
    debugS "target" target
    liftIO $ do
        result <- runIOorExplode (writeLaTeX def final)
        T.writeFile target result

renderPDF :: Program Env ()
renderPDF = do
    env <- getApplicationState

    let latex  = targetFileFrom env
        output = outputFileFrom env
        tmpdir = tempDirectoryFrom env

        latexmk = proc "latexmk"
            [ "-xelatex"
            , "-output-directory=" ++ tmpdir
            , "-interaction=nonstopmode"
            , "-halt-on-error"
            , "-file-line-error"
            , latex
            ]
        copy = proc "cp"
            [ output
            , "."
            ]

    debugS "output" output
    liftIO $ do
        runProcess_ (setStdin closed latexmk)
        runProcess_ (setStdin closed copy)
