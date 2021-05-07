{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CompareFragments
  ( checkByComparingFragments,
  )
where

import Core.Text
import qualified Data.Text.IO as T
import FormatDocument (markdownToPandoc)
import PandocToMarkdown (pandocToMarkdown)
import Test.Hspec hiding (context)

fragments :: [(String, FilePath)]
fragments =
  [ ("headings", "tests/fragments/Headings.markdown"),
    ("paragraphs", "tests/fragments/Paragraphs.markdown"),
    ("code blocks", "tests/fragments/CodeBlocks.markdown"),
    ("div blocks", "tests/fragments/DivBlocks.markdown"),
    ("LaTeX blocks", "tests/fragments/LatexBlocks.markdown"),
    ("HTML blocks", "tests/fragments/HtmlBlocks.markdown"),
    ("poem passage", "tests/fragments/Poetry.markdown"),
    ("blockquotes", "tests/fragments/Blockquotes.markdown"),
    ("bullet list", "tests/fragments/BulletList.markdown"),
    ("ordered list", "tests/fragments/OrderedList.markdown"),
    ("definition list", "tests/fragments/DefinitionList.markdown"),
    ("pipe table", "tests/fragments/PipeTable.md"),
    ("weird inlines", "tests/fragments/WeirdInlines.markdown")
  ]

checkByComparingFragments :: Spec
checkByComparingFragments =
  describe "Compare fragments" $ do
    sequence_ (map compareFragment fragments)

compareFragment :: (String, FilePath) -> SpecWith ()
compareFragment (label, file) =
  it ("Formats " ++ label ++ " correctly") $ do
    original <- T.readFile file
    doc <- markdownToPandoc original
    let text = pandocToMarkdown doc
    fromRope text `shouldBe` original
