{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Core.Program
import Core.Text
import Environment (initial)
import RenderDocument (program)

#ifdef __GHCIDE__
version :: Version
version = "0"
#else
version = $(fromPackage)
#endif

main :: IO ()
main = do
  env <- initial
  context <-
    configure
      version
      env
      ( simple
          [ Option
              "builtin-preamble"
              (Just 'p')
              Empty
              [quote|
            Wrap a built-in LaTeX preamble (and ending) around your
            supplied source fragments. Most documents will put their own
            custom preamble as the first fragment in the .book file, but
            for getting started a suitable default can be employed via this
            option.
          |],
            Option
              "watch"
              Nothing
              Empty
              [quote|
            Watch all sources listed in the bookfile and re-run the
            rendering engine if changes are detected.
          |],
            Option
              "temp"
              Nothing
              (Value "TMPDIR")
              [quote|
            The working location for assembling converted fragments and
            caching intermediate results between runs. By default, a
            temporary directory will be created in /tmp.
          |],
            Option
              "docker"
              Nothing
              (Value "IMAGE")
              [quote|
            Run the specified Docker image in a container, mount the target
            directory into it as a volume, and do the build there. This allows
            you to have all of the LaTeX dependencies separate from the machine
            you are editing on.
          |],
            Argument
              "bookfile"
              [quote|
            The file containing the list of fragments making up this book.
            If the argument is specified as "Hobbit.book" then "Hobbit"
            will be used as the basename for the final output .pdf file.
          |]
          ]
      )

  executeWith context program
