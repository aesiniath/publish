{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckTableProperties (
  checkTableProperties,
) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Core.Text
import FormatDocument (markdownToPandoc)
import PandocToMarkdown (
  NotSafe,
  pandocToMarkdown,
  tableToMarkdown,
 )
import Test.Hspec
import Text.Pandoc

notsafe :: Selector NotSafe
notsafe = const True

{-
tableToMarkdown ::
    Attr ->
    Caption ->
    [ColSpec] ->
    TableHead ->
    [TableBody] ->
    TableFoot ->
    Rope
-}
checkTableProperties :: Spec
checkTableProperties = do
  describe "Table rendering code" $ do
    it "Header rows format" $
      let result =
            tableToMarkdown
              ("", [], [])
              ( Caption
                  Nothing
                  []
              )
              [ (AlignRight, ColWidthDefault)
              , (AlignCenter, ColWidthDefault)
              , (AlignDefault, ColWidthDefault)
              ]
              ( TableHead
                  ("", [], [])
                  [ Row
                      ("", [], [])
                      [ Cell
                          ("", [], [])
                          AlignDefault
                          (RowSpan 1)
                          (ColSpan 1)
                          [Plain [Str "First"]]
                      , Cell
                          ("", [], [])
                          AlignDefault
                          (RowSpan 1)
                          (ColSpan 1)
                          [Plain [Str "Second"]]
                      , Cell
                          ("", [], [])
                          AlignDefault
                          (RowSpan 1)
                          (ColSpan 1)
                          [Plain [Str "Third"]]
                      ]
                  ]
              )
              [ ( TableBody
                    ("", [], [])
                    (RowHeadColumns 0)
                    []
                    [ Row
                        ("", [], [])
                        [ Cell
                            ("", [], [])
                            AlignDefault
                            (RowSpan 1)
                            (ColSpan 1)
                            [Plain [Str "1"]]
                        , Cell
                            ("", [], [])
                            AlignDefault
                            (RowSpan 1)
                            (ColSpan 1)
                            [Plain [Str "2"]]
                        , Cell
                            ("", [], [])
                            AlignDefault
                            (RowSpan 1)
                            (ColSpan 1)
                            [Plain [Str "3"]]
                        ]
                    ]
                )
              ]
              ( TableFoot
                  ("", [], [])
                  []
              )
       in do
            result
              `shouldBe` [quote|
| First | Second | Third |
|-------:|:------:|--------|
| 1 | 2 | 3 |
            |]
