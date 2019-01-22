{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckTableProperties
    ( checkTableProperties
    )
where

import Core.Text
import Test.Hspec

import PandocToMarkdown (NotSafe(..), rectanglerize)

checkTableProperties :: Spec
checkTableProperties = do
    describe "Table rendering code" $ do
        it "Making wrapped text into rectangles" $
            rectanglerize 6 "First Name" `shouldBe` ["First ","Name  "]
            
