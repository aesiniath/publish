{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckBookfileParser
    ( checkBookfileParser
    )
where

import Core.Text
import Test.Hspec
import Text.Megaparsec

import Environment (Bookfile(..))
import ParseBookfile

checkBookfileParser :: Spec
checkBookfileParser = do
    describe "Parse bookfile format" $ do
        it "Correctly parses a complete first line" $ do
            parseMaybe parseMagicLine "% publish v2\n" `shouldBe` Just 2
        it "Errors if first line has incorrect syntax" $ do
            parseMaybe parseMagicLine "%\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "%publish\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish \n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish  v2\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v1\n" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v2 asdf\n" `shouldBe` Nothing

        it "Correctly parses a preamble line" $ do
            parseMaybe parseFileLine "preamble.latex" `shouldBe` Just "preamble.latex"
        it "Parses two filenames in a list" $ do
            parseMaybe (many (parseFileLine <* parseBlank)) "one.markdown\ntwo.markdown"
                `shouldBe` Just (["one.markdown", "two.markdown"] :: [FilePath])

        it "Parses two filenames with a blank line between them" $ do
            parseMaybe (many (parseFileLine <* parseBlank)) [quote|
one.markdown

two.markdown
            |]
                `shouldBe` Just (["one.markdown", "two.markdown"] :: [FilePath])

        it "Correctly parses a begin end end pragmas" $ do
            parseMaybe parseBeginLine "% begin\n" `shouldBe` Just ()
            parseMaybe parseEndLine "% end\n" `shouldBe` Just ()

        it "correctly parses a complete bookfile" $ do
            parseMaybe parseBookfile [quote|
% publish v2
preamble.latex
% begin
Introduction.markdown
Conclusion.markdown
% end
            |] `shouldBe` Just (Bookfile 2 ["preamble.latex"] ["Introduction.markdown", "Conclusion.markdown"])

        it "correctly parses a complete bookfile with no preamble" $ do
            parseMaybe parseBookfile [quote|
% publish v2
% begin
Introduction.markdown
Conclusion.markdown
% end
            |] `shouldBe` Just (Bookfile 2 [] ["Introduction.markdown", "Conclusion.markdown"])
