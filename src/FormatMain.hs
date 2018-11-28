{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Core.Program
import Core.Text

import FormatDocument (program)

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    context <- configure version None (simple
        [ Argument "document" [quote|
            The file containing the markdown to be reformatted
          |]
        ])

    executeWith context program
