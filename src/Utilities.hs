{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utilities (
    ensureDirectory,
    ifNewer,
    isNewer,
) where

import Control.Monad (when)
import Core.Data
import Core.Program
import Core.System
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
 )
import System.FilePath.Posix (takeDirectory)

{-
Some source files live in subdirectories. Replicate that directory
structure in the temporary build space
-}
ensureDirectory :: FilePath -> Program t ()
ensureDirectory target =
    let subdir = takeDirectory target
     in liftIO $ do
            probe <- doesDirectoryExist subdir
            when (not probe) $ do
                createDirectoryIfMissing True subdir

{- |
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
    time2 <-
        doesFileExist target >>= \case
            True -> getModificationTime target
            False -> return (fromTime epochTime)
    return (time1 > time2)
