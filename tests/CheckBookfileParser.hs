{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckBookfileParser
    ( checkBookfileParser
    )
where

import Core.System
import Core.Text
import Test.Hspec
import Text.Megaparsec

import ParseBookfile

checkBookfileParser :: Spec
checkBookfileParser = do
    describe "Parse magic line" $ do
        it "correctly parses a complete first line" $ do
            parseMaybe parseMagicLine "% publish v0" `shouldBe` Just 0
        it "errors if missing syntax" $ do
            parseMaybe parseMagicLine "%" `shouldBe` Nothing
            parseMaybe parseMagicLine "%publish" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish " `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish  v0" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v1" `shouldBe` Nothing
            parseMaybe parseMagicLine "% publish v0 asdf" `shouldBe` Nothing

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
