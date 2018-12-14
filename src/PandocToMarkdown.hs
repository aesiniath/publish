{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PandocToMarkdown
    ( pandocToMarkdown
    )
where

import Core.Text
import Data.Foldable (foldl')
import Text.Pandoc (Pandoc(..), Block(..), Inline(..))

pandocToMarkdown :: Pandoc -> Rope
pandocToMarkdown (Pandoc _ blocks) =
    blocksToMarkdown blocks

blocksToMarkdown :: [Block] -> Rope
blocksToMarkdown blocks =
    foldl' (\text block -> append (convertBlock block) text) mempty blocks

convertBlock :: Block -> Rope
convertBlock block =
  let
    msg = "Unfinished block: " ++ show block
    result = case block of
        Plain inlines -> inlinesToMarkdown inlines
        Para  inlines -> wrap 75 (inlinesToMarkdown inlines)
        Header level _ inlines -> headingToMarkdown level inlines
        Null -> mempty
        RawBlock _ string -> intoRope string
        _ -> error msg
  in
    result <> "\n\n"

headingToMarkdown :: Int -> [Inline] -> Rope
headingToMarkdown level inlines =
  let
    text = inlinesToMarkdown inlines
  in
    case level of
        1 -> text <> "\n" <> underline '=' text
        2 -> text <> "\n" <> underline '-' text
        n -> intoRope (replicate n '#') <> " " <> text

inlinesToMarkdown :: [Inline] -> Rope
inlinesToMarkdown inlines =
    foldl' (\text inline -> append (convertInline inline) text) mempty inlines

convertInline :: Inline -> Rope
convertInline inline =
  let
    msg = "Unfinished inline: " ++ show inline
  in case inline of
    Space -> " "
    Str string -> intoRope string
    Emph inlines -> "_" <> inlinesToMarkdown inlines <> "_"
    Strong inlines -> "**" <> inlinesToMarkdown inlines <> "**"
    SoftBreak -> " "
    Image _ inlines (url, _) -> imageToMarkdown inlines url
    _ -> error msg

imageToMarkdown :: [Inline] -> String -> Rope
imageToMarkdown inlines url =
  let
    text = inlinesToMarkdown inlines
    target = intoRope url
  in
    "![" <> text <> "](" <> target <> ")"
    

