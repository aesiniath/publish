{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PandocToMarkdown
    ( pandocToMarkdown
    )
where

import Core.Text
import Data.Foldable (foldl')
import qualified Data.List as List
import Text.Pandoc (Pandoc(..), Block(..), Inline(..), Attr, Format(..))

pandocToMarkdown :: Pandoc -> Rope
pandocToMarkdown (Pandoc _ blocks) =
    blocksToMarkdown blocks

blocksToMarkdown :: [Block] -> Rope
blocksToMarkdown blocks =
    foldl' (\text block -> text <> (convertBlock 75 block) <> "\n") emptyRope blocks

convertBlock :: Int -> Block -> Rope
convertBlock margin block =
  let
    msg = "Unfinished block: " ++ show block -- FIXME
  in case block of
    Plain inlines -> paragraphToMarkdown margin inlines
    Para  inlines -> paragraphToMarkdown margin inlines
    Header level _ inlines -> headingToMarkdown level inlines
    Null -> emptyRope
    RawBlock (Format "tex") string -> intoRope string <> "\n"
    RawBlock _ _ -> error msg
    CodeBlock attr string -> codeToMarkdown attr string
    LineBlock list -> poemToMarkdown list
    BlockQuote blocks -> quoteToMarkdown margin blocks
    BulletList blockss -> listToMarkdown margin blockss
    _ -> error msg

plaintextToMarkdown :: Int -> [Inline] -> Rope
plaintextToMarkdown margin inlines =
    wrap margin (inlinesToMarkdown inlines)

paragraphToMarkdown :: Int -> [Inline] -> Rope
paragraphToMarkdown margin inlines =
    wrap margin (inlinesToMarkdown inlines) <> "\n"

headingToMarkdown :: Int -> [Inline] -> Rope
headingToMarkdown level inlines =
  let
    text = inlinesToMarkdown inlines
  in
    case level of
        1 -> text <> "\n" <> underline '=' text <> "\n"
        2 -> text <> "\n" <> underline '-' text <> "\n"
        n -> intoRope (replicate n '#') <> " " <> text <> "\n"

codeToMarkdown :: Attr -> String -> Rope
codeToMarkdown (_,tags,_) literal =
  let
    body = intoRope literal
    lang = case tags of
        []      -> ""
        [tag]   -> intoRope tag
        _       -> error "A code block can't have mulitple langage tags"
  in
    "```" <> lang <> "\n" <>
    body <> "\n" <>
    "```" <> "\n"

poemToMarkdown :: [[Inline]] -> Rope
poemToMarkdown list =
    mconcat (List.intersperse "\n" (fmap prefix list)) <> "\n"
  where
    prefix inlines = "| " <> inlinesToMarkdown inlines

quoteToMarkdown :: Int -> [Block] -> Rope
quoteToMarkdown margin blocks =
    foldl' (\text block -> text <> prefix block) emptyRope blocks
  where
    prefix :: Block -> Rope
    prefix = foldl' (\text line -> text <> "> " <> line <> "\n") emptyRope . rows

    rows :: Block -> [Rope]
    rows = breakLines . convertBlock (margin - 2)

listToMarkdown :: Int -> [[Block]] -> Rope
listToMarkdown margin items = case items of
    [] -> emptyRope
    (blocks1:blockss) -> listitem blocks1 <> foldl'
        (\text blocks -> text <> spacer blocks <> listitem blocks) emptyRope blockss
  where
    listitem :: [Block] -> Rope
    listitem [] = emptyRope
    listitem (block1:blocks) = indent True block1 <> foldl'
        (\ text blockN -> text <> indent False blockN) emptyRope blocks

    spacer :: [Block] -> Rope
    spacer [] = emptyRope
    spacer (block:_) = case block of
        Plain _ -> emptyRope
        Para _  -> "\n"
        _       -> "\n"

    indent :: Bool -> Block -> Rope
    indent first =
        snd . foldl' f (first,emptyRope) . breakLines . convertBlock (margin - 3)

    f :: (Bool,Rope) -> Rope -> (Bool,Rope)
    f (first,text) line = if first
        then (False,text <> " - " <> line <> "\n")
        else (False,text <> "   " <> line <> "\n")

----

inlinesToMarkdown :: [Inline] -> Rope
inlinesToMarkdown inlines =
    foldl' (\text inline -> append (convertInline inline) text) emptyRope inlines

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
    Code _ string -> "`" <> intoRope string <> "`"
    RawInline (Format "tex") string -> intoRope string
    _ -> error msg

imageToMarkdown :: [Inline] -> String -> Rope
imageToMarkdown inlines url =
  let
    text = inlinesToMarkdown inlines
    target = intoRope url
  in
    "![" <> text <> "](" <> target <> ")"
    
