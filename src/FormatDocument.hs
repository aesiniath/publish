{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FormatDocument (
    program,
    loadFragment,
    markdownToPandoc,
) where

import Core.Program
import Core.System
import Core.Text
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T
import PandocToMarkdown
import System.Directory (getFileSize, renameFile)
import Text.Pandoc (
    Extension (..),
    Extensions,
    Pandoc,
    ReaderOptions (readerExtensions),
    def,
    disableExtension,
    pandocExtensions,
    readMarkdown,
    runIOorExplode,
 )

program :: Program None ()
program = do
    info "Identify document fragment"
    file <- getFragmentName

    info "Load to Pandoc internal representation"
    parsed <- loadFragment file

    info "Write to Markdown format"
    writeResult file parsed

    info "Complete"

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
    let disableFrom :: Extensions -> [Extension] -> Extensions
        disableFrom extensions list = foldr disableExtension extensions list
        readingOptions =
            def
                { readerExtensions =
                    disableFrom
                        pandocExtensions
                        [ Ext_implicit_figures
                        , Ext_shortcut_reference_links
                        , Ext_smart
                        ]
                }
     in do
            runIOorExplode $ do
                readMarkdown readingOptions contents

data Inplace = Inplace | Console

writeResult :: FilePath -> Pandoc -> Program None ()
writeResult file doc =
    let contents' = pandocToMarkdown doc
        result = file ++ "~tmp"
     in do
            params <- getCommandLine

            let mode = case lookupOptionFlag "inplace" params of
                    Just False -> error "Invalid State"
                    Just True -> Inplace
                    Nothing -> Console

            case mode of
                Inplace -> liftIO $ do
                    withFile result WriteMode $ \handle ->
                        hWrite handle contents'

                    size <- getFileSize result
                    if size == 0
                        then error "Zero content, not overwriting"
                        else renameFile result file
                Console -> liftIO $ do
                    hWrite stdout contents'
