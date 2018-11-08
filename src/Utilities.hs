{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utilities
    ( ensureDirectory
    , execProcess
    )
where

import Control.Monad (when)
import Core.Program
import Core.System
import Core.Text
import qualified Data.List as List (intercalate)
import System.Directory (doesDirectoryExist, createDirectory)
import System.Exit (ExitCode(..))
import System.FilePath.Posix (takeDirectory)
import System.Process.Typed (proc, readProcess, setStdin, closed)

{-
Some source files live in subdirectories. Replicate that directory
structure in the temporary build space
-}
ensureDirectory :: FilePath -> Program a ()
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
execProcess :: [String] -> Program a (ExitCode, Rope, Rope)
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
