module Main where

import RenderDocument (render)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    render args
