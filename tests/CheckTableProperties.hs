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

import PandocToMarkdown (NotSafe, rectanglerize, combineRectangles)

notsafe :: Selector NotSafe
notsafe = const True

checkTableProperties :: Spec
checkTableProperties = do
    describe "Table rendering code" $ do
        it "Making wrapped text into rectangles" $
            rectanglerize 6 "First Name" `shouldBe`
                [ "First "
                , "Name  "
                ]

        it "Rectanglerizing a block with too long lines should throw" $
            (evaluate . force) (rectanglerize 7 "Borough of Kensington") `shouldThrow` notsafe

        it "Two rectangles of equal size combine" $
          let
            rect1 = rectanglerize 8 "This is a test"
            rect2 = rectanglerize 8 "Ode to Joy"
            result = combineRectangles rect1 rect2
          in do
            length rect1 `shouldBe` 2
            length rect2 `shouldBe` 2
            length result `shouldBe` 2
            result `shouldBe`
                [ "This is Ode to  "
                , "a test  Joy     "
                ]
 
        it "Two rectangles of unequal size combine (1)" $
          let
            --                        1234567890
            rect1 = rectanglerize 10 "Emergency Broadcast System"
            rect2 = rectanglerize 10 "Ode to Joy is nice"
            result = combineRectangles rect1 rect2
          in do
            length rect1 `shouldBe` 3
            length rect2 `shouldBe` 2
            length result `shouldBe` 3
            result `shouldBe`
                [ "Emergency Ode to Joy"
                , "Broadcast is nice   "
                , "System              "
                ]
            
        it "Two rectangles of unequal size combine (2)" $
          let
            --                        1234567890
            rect3 = rectanglerize 10 "This is an emergency"
            rect4 = rectanglerize 10 "Ode to Joy is nice piece that lots play"
            result = combineRectangles rect3 rect4
          in do
            length rect3 `shouldBe` 2
            length rect4 `shouldBe` 4
            length result `shouldBe` 4
            result `shouldBe`
                [ "This is anOde to Joy"
                , "emergency is nice   "
                , "          piece that"
                , "          lots play "
                ]

