{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core.Program
import FormatDocument (loadFragment)
import PandocToMarkdown (pandocToMarkdown)

main :: IO ()
main = execute $ do
  setVerbosityLevel Debug

  event "Load fragment"
  doc <- loadFragment "tests/fragments/PipeTable.md"
  debugS "doc" doc

  event "Convert..."
  let text = pandocToMarkdown doc
  debug "text" text

  event "Complete"
  terminate 0
