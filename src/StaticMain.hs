{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Core.Program
import Core.Text

import StaticDocument (program)

version :: Version
version = $(fromPackage)

main :: IO ()
main = do
    context <- configure version None (simple
        [ Argument "bookfile" [quote|
            The file containing the list of fragments making up this site.
            If the argument is specified as "Hobbit.book" then "Hobbit"
            will be used as the basename for the final output .html file.
          |]
        ])

    executeWith context program
