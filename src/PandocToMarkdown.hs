{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PandocToMarkdown (
    pandocToMarkdown,
    NotSafe (..),
    tableToMarkdown,
) where

import Core.System.Base
import Core.Text
import Data.Foldable (foldl')
import Data.List (intersperse)
import qualified Data.Text as T (Text, null)
import Text.Pandoc (
    Alignment (..),
    Attr,
    Block (..),
    Caption (..),
    Cell (..),
    ColSpan (..),
    ColSpec,
    ColWidth (..),
    Format (..),
    Inline (..),
    ListAttributes,
    MathType (..),
    Pandoc (..),
    QuoteType (..),
    Row (..),
    RowSpan (..),
    TableBody (..),
    TableFoot (..),
    TableHead (..),
 )
import Text.Pandoc.Shared (orderedListMarkers)

__WIDTH__ :: Int
__WIDTH__ = 78

pandocToMarkdown :: Pandoc -> Rope
pandocToMarkdown (Pandoc _ blocks) =
    blocksToMarkdown __WIDTH__ blocks

blocksToMarkdown :: Int -> [Block] -> Rope
blocksToMarkdown _ [] = emptyRope
blocksToMarkdown margin (block1 : blocks) =
    convertBlock margin block1
        <> foldl'
            (\text block -> text <> "\n" <> convertBlock margin block)
            emptyRope
            blocks

convertBlock :: Int -> Block -> Rope
convertBlock margin block =
    let msg = "Unfinished block: " ++ show block -- FIXME
     in case block of
            Plain inlines -> plaintextToMarkdown margin inlines
            Para inlines -> paragraphToMarkdown margin inlines
            Header level _ inlines -> headingToMarkdown level inlines
            Null -> emptyRope
            RawBlock (Format "tex") string -> intoRope string <> "\n"
            RawBlock (Format "html") string -> intoRope string <> "\n"
            RawBlock _ _ -> error msg
            CodeBlock attr string -> codeToMarkdown attr string
            LineBlock list -> poemToMarkdown list
            BlockQuote blocks -> quoteToMarkdown margin blocks
            BulletList blockss -> bulletlistToMarkdown margin blockss
            OrderedList attrs blockss -> orderedlistToMarkdown margin attrs blockss
            DefinitionList blockss -> definitionlistToMarkdown margin blockss
            HorizontalRule -> "---\n"
            Table attr caption alignments header rows footer -> tableToMarkdown attr caption alignments header rows footer
            Div attr blocks -> divToMarkdown margin attr blocks

{-
This does **not** emit a newline at the end. The intersperse happening in
`blocksToMarkdown` will terminate the line, but you won't get a blank line
between blocks as is the convention everywhere else (this was critical when
lists were nested in tight lists).
-}
plaintextToMarkdown :: Int -> [Inline] -> Rope
plaintextToMarkdown margin inlines =
    wrap' margin (inlinesToMarkdown inlines)

{-
Everything was great until we had to figure out how to deal with line
breaks aka <BR>, represented in Markdown by [' ',' ']. We do this by
replacing the line break Inline with \x2028. This character, U+2028 LS, is
the Line Separator character. It's one of those symbols up in General
Punctuation that no one ever uses. So we use it as a sentinel internally
here; first we break on those, and then we wrap the results.
-}
paragraphToMarkdown :: Int -> [Inline] -> Rope
paragraphToMarkdown margin inlines =
    wrap' margin (inlinesToMarkdown inlines) <> "\n"

wrap' :: Int -> Rope -> Rope
wrap' margin =
    mconcat . intersperse "  \n" . fmap (wrap margin) . breakPieces isLineSeparator
  where
    isLineSeparator = (== '\x2028')

headingToMarkdown :: Int -> [Inline] -> Rope
headingToMarkdown level inlines =
    let text = inlinesToMarkdown inlines
     in case level of
            1 -> text <> "\n" <> underline '=' text <> "\n"
            2 -> text <> "\n" <> underline '-' text <> "\n"
            n -> intoRope (replicate n '#') <> " " <> text <> "\n"

codeToMarkdown :: Attr -> T.Text -> Rope
codeToMarkdown attr literal =
    let body = intoRope literal
        lang = fencedAttributesToMarkdown attr
     in "```" <> lang <> "\n"
            <> body
            <> "\n"
            <> "```"
            <> "\n"

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
bulletlistToMarkdown = listToMarkdown (repeat "-   ")

orderedlistToMarkdown :: Int -> ListAttributes -> [[Block]] -> Rope
orderedlistToMarkdown margin (num, style, delim) blockss =
    listToMarkdown (intoMarkers (num, style, delim)) margin blockss
  where
    intoMarkers = fmap pad . fmap intoRope . orderedListMarkers
    pad text = text <> if widthRope text > 2 then " " else "  "

definitionlistToMarkdown :: Int -> [([Inline], [[Block]])] -> Rope
definitionlistToMarkdown margin definitions =
    case definitions of
        [] -> emptyRope
        (definition1 : definitionN) ->
            handleDefinition definition1
                <> foldl'
                    (\text definition -> text <> "\n" <> handleDefinition definition)
                    emptyRope
                    definitionN
  where
    handleDefinition :: ([Inline], [[Block]]) -> Rope
    handleDefinition (term, blockss) =
        inlinesToMarkdown term <> "\n\n" <> listToMarkdown (repeat ":   ") margin blockss

listToMarkdown :: [Rope] -> Int -> [[Block]] -> Rope
listToMarkdown markers margin items =
    case pairs of
        [] -> emptyRope
        ((marker1, blocks1) : pairsN) ->
            listitem marker1 blocks1
                <> foldl'
                    (\text (markerN, blocksN) -> text <> spacer blocksN <> listitem markerN blocksN)
                    emptyRope
                    pairsN
  where
    pairs = zip markers items
    listitem :: Rope -> [Block] -> Rope
    listitem _ [] = emptyRope
    listitem marker blocks = indent marker blocks
    {-
    Tricky. Tight lists are represented by Plain, whereas more widely spaced
    lists are represented by Para. A complex block (specifically a nested
    list!) will handle its own spacing. This seems fragile.
    -}
    spacer :: [Block] -> Rope
    spacer [] = emptyRope
    spacer (block : _) = case block of
        Plain _ -> emptyRope
        Para _ -> "\n"
        _ -> emptyRope -- ie nested list
    indent :: Rope -> [Block] -> Rope
    indent marker =
        snd . foldl' (f marker) (True, emptyRope) . breakLines . blocksToMarkdown (margin - 4)
    f :: Rope -> (Bool, Rope) -> Rope -> (Bool, Rope)
    f marker (first, text) line
        | nullRope line =
            (False, text <> "\n") -- don't indent lines that should be blank
        | otherwise =
            if first
                then (False, text <> marker <> line <> "\n")
                else (False, text <> "    " <> line <> "\n")

{-
In Pandoc flavoured Markdown, <div> are recognized as valid Markdown via
the `native_divs` extension. We turn that off, in favour of the
`fenced_divs` extension, three (or more) colons

    ::: {#identifier .class key=value}
    Content
    :::

-}
divToMarkdown :: Int -> Attr -> [Block] -> Rope
divToMarkdown margin attr blocks =
    let first = ":::" <> fencedAttributesToMarkdown attr
        trail = ":::"
        content = mconcat . intersperse "\n" . fmap (convertBlock margin)
     in first <> "\n" <> content blocks <> trail <> "\n"

-- special case for (notably) code blocks where a single class doesn't need braces.
fencedAttributesToMarkdown :: Attr -> Rope
fencedAttributesToMarkdown ("", [], []) = emptyRope
fencedAttributesToMarkdown ("", [single], []) = intoRope single
fencedAttributesToMarkdown (identifier, [], []) = " " <> attributesToMarkdown (identifier, [], [])
fencedAttributesToMarkdown (identifier, classes, pairs) = " " <> attributesToMarkdown (identifier, classes, pairs)

-- present attributes, used by both fenced blocks and inline spans
attributesToMarkdown :: Attr -> Rope
attributesToMarkdown ("", [], []) = emptyRope
attributesToMarkdown (identifier, [], []) = "{#" <> intoRope identifier <> "}"
attributesToMarkdown (identifier, classes, pairs) =
    let i =
            if T.null identifier
                then emptyRope
                else "#" <> intoRope identifier <> " "
        cs = fmap (\c -> "." <> intoRope c) classes
        ps = fmap (\(k, v) -> intoRope k <> "=" <> intoRope v) pairs
     in "{" <> i <> mconcat (intersperse " " (cs ++ ps)) <> "}"

tableToMarkdown ::
    Attr ->
    Caption ->
    [ColSpec] ->
    TableHead ->
    [TableBody] ->
    TableFoot ->
    Rope
tableToMarkdown _ _ alignments thead tbodys _ =
    mconcat
        ( intersperse
            "\n"
            [ headerline
            , betweenline
            , bodylines
            ]
        )
        <> "\n"
  where
    colonChar = singletonRope ':'
    dashChar = singletonRope '-'
    pipeChar = singletonRope '|'
    spaceChar = singletonRope ' '
    newlineChar = singletonRope '\n'

    surround :: Rope -> Rope -> Rope
    surround char text = char <> text <> char

    headerline = headerToMarkdown thead

    betweenline =
        surround pipeChar . foldl' (<>) emptyRope
            . intersperse pipeChar
            . fmap columnToMarkdown
            $ alignments

    bodylines = bodiesToMarkdown tbodys

    headerToMarkdown :: TableHead -> Rope
    headerToMarkdown (TableHead _ [row]) = rowToMarkdown row
    headerToMarkdown _ = impureThrow (NotSafe "What do we do with this TableHead?")

    columnToMarkdown :: (Alignment, ColWidth) -> Rope
    columnToMarkdown (align, col) =
        let total = fromIntegral __WIDTH__
            begin = case align of
                AlignLeft -> colonChar
                AlignCenter -> colonChar
                _ -> dashChar

            num = case col of
                ColWidth x -> floor (total * x) - 2
                ColWidthDefault -> 6
            middle = mconcat (replicate num dashChar)

            end = case align of
                AlignRight -> colonChar
                AlignCenter -> colonChar
                _ -> dashChar
         in begin <> middle <> end

    bodiesToMarkdown :: [TableBody] -> Rope
    bodiesToMarkdown = mconcat . intersperse newlineChar . fmap bodyToMarkdown

    bodyToMarkdown :: TableBody -> Rope
    bodyToMarkdown (TableBody _ _ _ rows) =
        foldl' (<>) emptyRope
            . intersperse newlineChar
            . fmap rowToMarkdown
            $ rows

    rowToMarkdown :: Row -> Rope
    rowToMarkdown (Row _ cells) =
        surround pipeChar . foldl' (<>) emptyRope
            . intersperse pipeChar
            . fmap (surround spaceChar . cellToMarkdown)
            $ cells

    cellToMarkdown :: Cell -> Rope
    cellToMarkdown (Cell _ _ (RowSpan 1) (ColSpan 1) [block]) =
        convert block
    cellToMarkdown _ =
        impureThrow (NotSafe "Multiple Blocks encountered")

    convert :: Block -> Rope
    convert (Plain inlines) =
        plaintextToMarkdown 100000 inlines
    convert _ =
        impureThrow (NotSafe "Incorrect Block type encountered")

data NotSafe = NotSafe String
    deriving (Show)

instance Exception NotSafe

---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

inlinesToMarkdown :: [Inline] -> Rope
inlinesToMarkdown inlines =
    foldl' (\text inline -> appendRope (convertInline inline) text) emptyRope inlines

convertInline :: Inline -> Rope
convertInline inline =
    let msg = "Unfinished inline: " ++ show inline
     in case inline of
            Space -> " "
            Str text -> stringToMarkdown text
            Emph inlines -> "_" <> inlinesToMarkdown inlines <> "_"
            Strong inlines -> "**" <> inlinesToMarkdown inlines <> "**"
            SoftBreak -> " "
            LineBreak -> "\x2028"
            Image attr inlines target -> imageToMarkdown attr inlines target
            Code _ string -> "`" <> intoRope string <> "`"
            RawInline (Format "tex") string -> intoRope string
            RawInline (Format "html") string -> intoRope string
            RawInline _ _ -> error msg
            Link ("", ["uri"], []) _ (url, _) -> uriToMarkdown url
            Link attr inlines target -> linkToMarkdown attr inlines target
            Strikeout inlines -> "~~" <> inlinesToMarkdown inlines <> "~~"
            Math mode text -> mathToMarkdown mode text
            -- then things start getting weird
            SmallCaps inlines -> smallcapsToMarkdown inlines
            Subscript inlines -> "~" <> inlinesToMarkdown inlines <> "~"
            Superscript inlines -> "^" <> inlinesToMarkdown inlines <> "^"
            Span attr inlines -> spanToMarkdown attr inlines
            -- I don't know what the point of these ones are
            Quoted SingleQuote inlines -> "'" <> inlinesToMarkdown inlines <> "'"
            Quoted DoubleQuote inlines -> "\"" <> inlinesToMarkdown inlines <> "\""
            _ -> error msg

{-
Pandoc uses U+00A0 aka ASCII 160 aka &nbsp; to mark a non-breaking space, which
seems to be how it describes an escaped space in Markdown. So scan for these
and replace the escaped space on output.
-}
stringToMarkdown :: T.Text -> Rope
stringToMarkdown =
    escapeSpecialWith '\x00a0' ' '
        . escapeSpecial '['
        . escapeSpecial ']'
        . escapeSpecial '_'
        . intoRope

escapeSpecial :: Char -> Rope -> Rope
escapeSpecial c = escapeSpecialWith c c

escapeSpecialWith :: Char -> Char -> Rope -> Rope
escapeSpecialWith needle replacement =
    mconcat . intersperse (singletonRope '\\' <> singletonRope replacement) . breakPieces isNeedle . intoRope
  where
    isNeedle c = c == needle

imageToMarkdown :: Attr -> [Inline] -> (T.Text, T.Text) -> Rope
imageToMarkdown attr inlines (url, title) =
    let alt = inlinesToMarkdown inlines
        target =
            if T.null title
                then intoRope url
                else intoRope url <> " \"" <> intoRope title <> "\""
     in "![" <> alt <> "](" <> target <> ")" <> attributesToMarkdown attr

uriToMarkdown :: T.Text -> Rope
uriToMarkdown url =
    let target = intoRope url
     in "<" <> target <> ">"

linkToMarkdown :: Attr -> [Inline] -> (T.Text, T.Text) -> Rope
linkToMarkdown attr inlines (url, title) =
    let text = inlinesToMarkdown inlines
        target =
            if T.null title
                then intoRope url
                else intoRope url <> " \"" <> intoRope title <> "\""
     in "[" <> text <> "](" <> target <> ")" <> attributesToMarkdown attr

-- is there more to this?
mathToMarkdown :: MathType -> T.Text -> Rope
mathToMarkdown (InlineMath) math = "$" <> intoRope math <> "$"
mathToMarkdown (DisplayMath) math = "$$" <> intoRope math <> "$$"

smallcapsToMarkdown :: [Inline] -> Rope
smallcapsToMarkdown inlines =
    let text = inlinesToMarkdown inlines
     in "[" <> text <> "]{.smallcaps}"

spanToMarkdown :: Attr -> [Inline] -> Rope
spanToMarkdown attr inlines =
    let text = inlinesToMarkdown inlines
     in "[" <> text <> "]" <> attributesToMarkdown attr
