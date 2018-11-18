{-# LANGUAGE OverloadedStrings #-}

module NotifyChanges
(
    waitForChange
)
where

import Core.Program
import Core.System
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import qualified Data.ByteString.Char8 as C (pack)
import System.INotify (EventVariety(..), Event(..), withINotify
    , addWatch, removeWatch)

import Environment

waitForChange :: Program Env () 
waitForChange = do
    env <- getApplicationState
    let files = intermediateFilenamesFrom env

    withContext $ \runProgram -> do
        block <- newEmptyMVar
        withINotify $ \notify -> do
            -- setup inotifies
            watches <- mapM (\file -> do
                addWatch
                    notify
                    [CloseWrite]
                    (C.pack file)
                    (\event -> case event of
                        Closed _ path _  -> do
                            runProgram (debugS "path" path)
                            putMVar block False
                        _ -> return ()))
                files

            -- wait
            readMVar block
            -- cleanup
            mapM_ removeWatch watches

