module NotifyChanges
(
    waitForChange
)
where

import Core.Program
import Core.System
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import System.INotify (EventVariety(..), Event(..), withINotify
    , addWatch, removeWatch)

import Environment

waitForChange :: Program Env () 
waitForChange = do
    withContext $ \runProgram -> do
        block <- newEmptyMVar
        withINotify $ \notify -> do
            watch <- addWatch
                notify
                [CloseWrite]
                "."
                (\event -> case event of
                    Closed _ path _  -> do
                        runProgram (debugS "path" path)
                        putMVar block False
                    _ -> return ())
            -- wait
            readMVar block
            removeWatch watch


