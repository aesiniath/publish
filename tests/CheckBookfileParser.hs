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

import ParseBookfile

checkBookfileParser :: Spec
checkBookfileParser = do
    describe "Parse magic line" $ do
        it "correctly parses a complete first line" $ do
            parseMaybe parseMagicLine "% publish v2" `shouldBe` Just 2
        it "errors if missing syntax" $ do
            parseMaybe parseMagicLine "%" `shouldBe` Nothing
            parseMaybe parseMagicLine "%publish" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish " `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish  v2" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v1" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v2 asdf" `shouldBe` Nothing

    describe "Parse fragment lines" $ do
        it "correctly parses a preamble line" $ do
            parseMaybe parseFileLine "preamble.latex" `shouldBe` Just "preamble.latex"
        it "parses two filenames in a list" $ do
            parseMaybe (many (parseFileLine <* parseBlank)) "one.markdown\ntwo.markdown"
                `shouldBe` Just (["one.markdown", "two.markdown"] :: [FilePath])

        it "parses two filenames with a blank line between them" $ do
            parseMaybe (many (parseFileLine <* parseBlank)) [quote|
one.markdown

two.markdown
            |]
                `shouldBe` Just (["one.markdown", "two.markdown"] :: [FilePath])

    describe "Complete document" $ do
        it "correctly parses a complete bookfile" $ do
            parseMaybe parseBookfile [quote|
% publish v2
preamble.latex
% begin
Introduction.markdown
Conclusion.markdown
% end
            |] `shouldBe` Just (Bookfile 2 ["preamble.latex"] ["Introduction.markdown", "Conclusion.markdown"])