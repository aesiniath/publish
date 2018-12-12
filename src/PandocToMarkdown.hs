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
    msg = "Unfinished block conversion: " ++ show block
  in case block of
    Plain inlines -> inlinesToMarkdown inlines
    Para  inlines -> inlinesToMarkdown inlines
    Null -> mempty
    _ -> error msg

inlinesToMarkdown :: [Inline] -> Rope
inlinesToMarkdown inlines =
    foldl' (\text inline -> append (convertInline inline) text) mempty inlines

convertInline :: Inline -> Rope
convertInline inline =
  let
    msg = "Unfinished inline conversion: " ++ show inline
  in case inline of
    Space -> " "
    Str string -> intoRope string
    Emph inlines -> "_" <> inlinesToMarkdown inlines <> "_"
    Strong inlines -> "**" <> inlinesToMarkdown inlines <> "**"
    _ -> error msg

    

