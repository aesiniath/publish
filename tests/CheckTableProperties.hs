{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckTableProperties
    ( checkTableProperties
    )
where

import Core.Text
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Test.Hspec

import PandocToMarkdown (NotSafe, rectanglerize)

notsafe :: Selector NotSafe
notsafe = const True

checkTableProperties :: Spec
checkTableProperties = do
    describe "Table rendering code" $ do
        it "Making wrapped text into rectangles" $
            rectanglerize 6 "First Name" `shouldBe` ["First ","Name  "]

        it "Rectanglerizing a block with too long lines should throw" $
            (evaluate . force) (rectanglerize 7 "Borough of Kensington") `shouldThrow` notsafe
            
