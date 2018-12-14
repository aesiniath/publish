{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FormatDocument
    ( program
    , loadFragment
    )
where

import Core.Program
import Core.System
import Core.Text

import qualified Data.Text.IO as T
import System.IO (withFile, IOMode(..))
import Text.Pandoc (runIOorExplode, readMarkdown, writeMarkdown, def
    , readerExtensions, pandocExtensions, writerExtensions, writerColumns
    , writerSetextHeaders, writerWrapText, WrapOption(WrapAuto), Pandoc)

import PandocToMarkdown

program :: Program None ()
program = do
    event "Identify document fragment"
    file <- getFragmentName

    event "Load to Pandoc internal representation"
    parsed <- loadFragment file

    event "Write to Markdown format"
    writeResult file parsed

    event "Complete"

getFragmentName :: Program None FilePath
getFragmentName = do
    params <- getCommandLine
    let fragment = case lookupArgument "document" params of
            Nothing -> error "invalid"
            Just file -> file
    return fragment

loadFragment :: FilePath -> Program None Pandoc
loadFragment file =
  let
    readingOptions = def
        { readerExtensions = pandocExtensions
        }
  in
    liftIO $ do
        contents <- T.readFile file
        runIOorExplode $ do
            readMarkdown readingOptions contents

writeResult :: FilePath -> Pandoc -> Program None ()
writeResult file doc =
  let
    result = file ++ "-tmp"
    contents' = pandocToMarkdown doc
  in
    liftIO $ do
        withFile result WriteMode $ \handle ->
            hWrite handle contents'

