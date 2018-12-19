{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FormatDocument
    ( program
    , loadFragment
    , markdownToPandoc
    )
where

import Core.Program
import Core.System
import Core.Text

import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T
import System.IO (withFile, IOMode(..))
import Text.Pandoc (runIOorExplode, readMarkdown, writeMarkdown, def
    , readerExtensions, pandocExtensions, disableExtension, Extension(..)
    , Pandoc)

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
    liftIO $ do
        contents <- T.readFile file
        markdownToPandoc contents

--
-- Unlike the render use case, here we suppress certain
-- options which mess up the ASCII form of the source documents
--
markdownToPandoc :: T.Text -> IO Pandoc
markdownToPandoc contents =
  let
    extensions = disableExtension Ext_smart pandocExtensions
    readingOptions = def
        { readerExtensions = extensions
        }
  in do
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

