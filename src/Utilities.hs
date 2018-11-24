{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Utilities
    ( ensureDirectory
    , execProcess
    , ifNewer
    , isNewer
    )
where

import Chrono.Compat (convertToUTC)
import Control.Monad (when)
import Core.Program
import Core.System
import Core.Text
import qualified Data.List as List (intercalate)
import System.Directory (doesDirectoryExist, doesFileExist, createDirectory
    , getModificationTime, copyFileWithMetadata)
import System.Exit (ExitCode(..))
import System.FilePath.Posix (takeDirectory)
import System.Process.Typed (proc, readProcess, setStdin, closed)

{-
Some source files live in subdirectories. Replicate that directory
structure in the temporary build space
-}
ensureDirectory :: FilePath -> Program t ()
ensureDirectory target =
  let
     subdir = takeDirectory target
  in liftIO $ do
    probe <- doesDirectoryExist subdir
    when (not probe) $ do
        createDirectory subdir

{-
Thin wrapper around **typed-process**'s `readProcess` so that the command
to be executed can be logged. Bit of an annoyance that the command and the
arguments have to be specified to `proc` separately, but that's _execvp(3)_
for you.

TODO this could potentially move to the **unbeliever** library
-}
execProcess :: [String] -> Program t (ExitCode, Rope, Rope)
execProcess [] = error "No command provided"
execProcess (cmd:args) =
  let
    task = proc cmd args
    task' = setStdin closed task
    command = List.intercalate " " (cmd:args)
  in do
    debugS "command" command

    (exit, out, err) <- liftIO $ do
        readProcess task'

    return (exit, intoRope out, intoRope err)

{-|
If the source file is newer than the target file, then run an action. For
example, if you want to install a file but only do so if the file has been
rebuilt, then you could do this:

@
copyFileIfNewer :: 'FilePath' -> 'FilePath' -> 'Program' τ ()
copyFileIfNewer source target = do
    'ifNewer' source target $ do
        'liftIO' ('copyFileWithMetadata' source target)
@

This is basically a build system in a box, although the usual caveats
about the brittleness of timestamps apply.

TODO this could potentially move to the **unbeliever** library
-}
ifNewer :: FilePath -> FilePath -> Program t () -> Program t ()
ifNewer source target program = do
    changed <- isNewer source target
    when changed $ do
        program

isNewer :: FilePath -> FilePath -> Program t Bool
isNewer source target = liftIO $ do
    time1 <- getModificationTime source
    time2 <- doesFileExist target >>= \case
        True  -> getModificationTime target
        False -> return (convertToUTC 0)        -- the epoch
    return (time1 > time2)
