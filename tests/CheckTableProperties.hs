{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckTableProperties (
    checkTableProperties,
) where

import Core.Text
import PandocToMarkdown (
    tableToMarkdown,
 )
import Test.Hspec
import Text.Pandoc

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
                        , (AlignDefault, ColWidth 0.5)
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
|-------:|:------:|---------------------------------------|
| 1 | 2 | 3 |
            |]
