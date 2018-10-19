module Main where

import Core.Program
import RenderDocument (program)


main :: IO ()
main = execute program
