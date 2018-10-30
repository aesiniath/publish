{-# LANGUAGE OverloadedStrings #-}

module OutputParser
    ( parseOutputForError
    )
where

import Core.Text
import qualified Data.ByteString.Lazy.Char8 as L

--
-- The build command returned a non-zero exit code, so there is a
-- reasonable assumption that there is indeed an error to be extracted.
--
parseOutputForError :: FilePath -> L.ByteString -> Rope
parseOutputForError file =
  let
    needle = L.snoc (L.pack file)  ':'

    stripBeginning [] = []
    stripBeginning (b:bs) = if L.isPrefixOf needle b
        then b : bs
        else stripBeginning bs

    dropEnding [] = []
    dropEnding (b:bs) = if L.isPrefixOf "Output written on" b || "No pages of output." == b
        then []
        else b : dropEnding bs
  in
    intoRope . L.intercalate "\n" . dropEnding . stripBeginning . L.lines


-- Error stream from xelatex looks like this:
{-
/tmp/publish-Km3eN1/Junk.tex:8: Undefined control sequence.
l.8 \broken
           
No pages of output.
Transcript written on /tmp/publish-Km3eN1/Junk.log
-}
