module NotifyChanges
(
    waitForChange
)
where

import Core.Program
import Core.System
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import System.FSNotify (Event(..), eventPath, withManager, watchTree)

import Environment

waitForChange :: Program Env () 
waitForChange = do
    withContext $ \runProgram -> do
        block <- newEmptyMVar
        withManager $ \manager -> do
            stop <- watchTree manager 
                "."
                (\event -> case event of
                    Modified _ _ False  -> True
                    _                   -> False)
                (\event -> do
                    runProgram (debugS "path" (eventPath event))
                    putMVar block False)

            -- wait
            readMVar block
            stop


