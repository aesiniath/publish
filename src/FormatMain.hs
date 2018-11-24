{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Core.Program
import Core.Text

import FormatDocument (program)
import Paths_publish (version)


main :: IO ()
main = do
    context <- configure (fromPackage version) None (simple
        [ Argument "document" [quote|
            The file containing the markdown to be reformatted
          |]
        ])

    executeWith context program
