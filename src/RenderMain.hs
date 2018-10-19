{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Core.Program
import Core.Text
import RenderDocument (program)


main :: IO ()
main = do
    context <- configure None (simple
        [ Argument "bookfile" [quote|
            The file containing the list of fragments making up this book.
          |]
        ])

    executeWith context program
