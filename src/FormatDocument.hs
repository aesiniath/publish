{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FormatDocument
    ( program
    )
where

import Core.Program
import Core.System
import Core.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import System.IO.Error (userError, IOError)
import System.Posix.Temp (mkdtemp)
import Text.Pandoc (runIOorExplode, readMarkdown, writeMarkdown, def
    , readerExtensions, pandocExtensions, writerTopLevelDivision
    , TopLevelDivision(TopLevelChapter))

program :: Program None ()
program = do
    params <- getCommandLine

    event "Identify document fragment"

    let file = case lookupArgument "document" params of
            Nothing -> error "invalid"
            Just file -> file

    return ()




