{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module PandocToMarkdown
    ( pandocToMarkdown
    , NotSafe(..)
    , Rectangle(..)
    , rectanglerize
    , combineRectangles
    , buildRow
    , widthOf
    , heightOf
    , tableToMarkdown
    )
where

import Control.DeepSeq (NFData)
import Core.Text
import Core.System
import Data.Foldable (foldl')
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.List (intersperse)
import GHC.Generics (Generic)
import Text.Pandoc (Pandoc(..), Block(..), Inline(..), Attr, Format(..)
    , ListAttributes, Alignment(..), TableCell)
import Text.Pandoc.Shared (orderedListMarkers)

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
    BulletList blockss -> bulletlistToMarkdown margin blockss
    OrderedList attrs blockss -> orderedlistToMarkdown margin attrs blockss
    HorizontalRule -> "---\n"
    Table caption alignments sizes headers rows -> tableToMarkdown caption alignments sizes headers rows
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
        _       -> impureThrow (NotSafe "A code block can't have mulitple langage tags")
  in
    "```" <> lang <> "\n" <>
    body <> "\n" <>
    "```" <> "\n"

poemToMarkdown :: [[Inline]] -> Rope
poemToMarkdown list =
    mconcat (intersperse "\n" (fmap prefix list)) <> "\n"
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

bulletlistToMarkdown :: Int -> [[Block]] -> Rope
bulletlistToMarkdown = listToMarkdown (repeat " -  ")

orderedlistToMarkdown :: Int -> ListAttributes -> [[Block]] -> Rope
orderedlistToMarkdown margin (num,style,delim) blockss =
  let
    intoMarkers = fmap (\text -> " " <> text <> " ") . fmap intoRope . orderedListMarkers
  in
    listToMarkdown (intoMarkers (num,style,delim)) margin blockss

listToMarkdown :: [Rope] -> Int -> [[Block]] -> Rope
listToMarkdown markers margin items =
  case pairs of
    [] -> emptyRope
    ((marker1,blocks1):pairsN) -> listitem marker1 blocks1 <> foldl'
        (\text (markerN,blocksN) -> text <> spacer blocksN <> listitem markerN blocksN) emptyRope pairsN
  where
    pairs = zip markers items

    listitem :: Rope -> [Block] -> Rope
    listitem _ [] = emptyRope
    listitem marker (block1:blocks) = indent marker True block1 <> foldl'
        (\ text blockN -> text <> indent marker False blockN) emptyRope blocks

    spacer :: [Block] -> Rope
    spacer [] = emptyRope
    spacer (block:_) = case block of
        Plain _ -> emptyRope
        Para _  -> "\n"
        _       -> "\n"

    indent :: Rope -> Bool -> Block -> Rope
    indent marker first =
        snd . foldl' (f marker) (first,emptyRope) . breakLines . convertBlock (margin - 4)

    f :: Rope -> (Bool,Rope) -> Rope -> (Bool,Rope)
    f marker (first,text) line = if first
        then (False,text <> marker <> line <> "\n")
        else (False,text <> "    " <> line <> "\n")


-- Assumtions:
-- 1. all the lists are the same length
-- 2. headers widths will be in increments of 5 characters?
-- 3. no complex tables
tableToMarkdown
    :: [Inline]
    -> [Alignment]
    -> [Double]
    -> [TableCell]
    -> [[TableCell]]
    -> Rope
tableToMarkdown caption alignments sizes headers rows =
    wrapperLine <> "\n"
    <> header <> "\n"
    <> underlineHeaders <> "\n"
    <> wrapperLine <> "\n"
  where
    header = buildRow blockSizes (headerToMarkdown headers)

    overall = sum blockSizes + (length headers) - 1
    wrapperLine = intoRope (replicate overall '-')

    headerToMarkdown :: [TableCell] -> [Rectangle]
    headerToMarkdown = fmap convertHeaderToRectangle . headerSizes blockSizes 

    underlineHeaders :: Rope
    underlineHeaders =
        foldl' (<>) emptyRope . intersperse " "
        . fmap (\size -> intoRope (replicate size '-'))
        . take (length headers) $ blockSizes

    convertHeaderToRectangle :: (Int,Block) -> Rectangle
    convertHeaderToRectangle (size,Plain inlines) =
        rectanglerize size (plaintextToMarkdown size inlines)
    convertHeaderToRectangle (size,_) =
        impureThrow (NotSafe "Incorrect Block type encountered")

    -- FIXME single block only, else throw exception
    headerSizes :: [Int] -> [[Block]] -> [(Int,Block)]
    headerSizes sizes headers = zipWith (\size (block:_) -> (size,block)) sizes headers

    blockSizes :: [Int]
    blockSizes = take (length headers) (repeat 15) -- FIXME


data NotSafe = NotSafe String
    deriving Show

instance Exception NotSafe

data Rectangle = Rectangle Int Int [Rope]
    deriving (Eq, Show, Generic, NFData)

widthOf :: Rectangle -> Int
widthOf (Rectangle size _ _) = size

heightOf :: Rectangle -> Int
heightOf (Rectangle _ height _) = height

rowsFrom :: Rectangle -> [Rope]
rowsFrom (Rectangle _ _ texts) = texts

instance Semigroup Rectangle where
    (<>) = combineRectangles

instance Monoid Rectangle where
    mempty = Rectangle 0 0 []

rectanglerize :: Int -> Rope -> Rectangle
rectanglerize size text =
  let
    ls = breakLines (wrap size text)

    fix l | width l <  size = l <> intoRope (replicate (size - width l) ' ')
          | width l == size = l
          | otherwise       = impureThrow (NotSafe "Line wider than permitted size")

    result = foldr (\l text -> fix l:text) [] ls
  in
    Rectangle size (length result) result

combineRectangles :: Rectangle -> Rectangle -> Rectangle
combineRectangles rect1@(Rectangle size1 height1 texts1) rect2@(Rectangle size2 height2 texts2) =
  let
    target = max height1 height2
    extra1 = target - height1
    extra2 = target - height2

    padRows :: Int -> Rectangle -> [Rope]
    padRows count (Rectangle size _ texts) =
      let
        texts' = texts ++ replicate count (intoRope (replicate size ' '))
      in
        texts'

    texts1' = padRows extra1 rect1
    texts2' = padRows extra2 rect2

    pairs = zip texts1' texts2'

    result = foldr (\ (text1,text2) texts -> text1 <> text2 : texts) [] pairs
  in
    Rectangle (size1 + size2) target  result

ensureWidth :: Int -> Rectangle -> Rectangle
ensureWidth request rect@(Rectangle size height texts) =
    if widthOf rect < request
        then rectanglerize request (foldl' (<>) emptyRope texts)
        else rect


buildRow :: [Int] -> [Rectangle] -> Rope
buildRow cellWidths rects =
  let
    pairs = zip cellWidths rects
    rects' = fmap (\ (desired,rect) -> ensureWidth desired rect) pairs
    wall = vertical ' ' rects'
    result = foldl' (<>) mempty . intersperse wall $ rects'
  in
    foldl' (<>) emptyRope (intersperse "\n" (rowsFrom result))

vertical :: Char -> [Rectangle] -> Rectangle
vertical ch rects =
  let
    height = maximum (fmap heightOf rects)
    border = replicate height (intoRope [ch])
  in
    Rectangle 1 height border

---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

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
    
