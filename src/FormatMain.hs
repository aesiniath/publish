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
        [ Option "inplace" (Just 'i') Empty [quote|
            Overwrite the original file with the reformatted version. WARNING
            This tool is experimental. You should ensure you have a safe copy
            of your original (ie, add it to Git's index) before running with
            this option enabled.
          |]
        , Argument "document" [quote|
            The file containing the markdown to be reformatted
          |]
        ])

    executeWith context program
