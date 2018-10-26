{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Core.Program
import Core.Text
import RenderDocument (program, initial)


main :: IO ()
main = do
    context <- configure initial (simple
        [ Argument "bookfile" [quote|
            The file containing the list of fragments making up this book.
            If the argument is specified as "Hobbit.book" then "Hobbit"
            will be used as the basename for the intermediate .latex file
            and the final output .pdf file. The list file should contain
            filenames, one per line, of the fragments you wish to render
            into a complete document.
          |]
        ])

    executeWith context program
