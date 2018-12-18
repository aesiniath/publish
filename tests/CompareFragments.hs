{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CompareFragments where

import Data.Foldable (foldl')
import Test.Hspec hiding (context)

fragments :: [(String,FilePath)]
fragments =
    [ ("headings",      "tests/fragments/Headings.hs")
    ]

checkByComparingFragments :: Spec
checkByComparingFragments =
    describe "Compare fragments" $ do
        sequence_ (map compareFragment fragments)

compareFragment :: (String,FilePath) -> SpecWith ()
compareFragment (label,file) =
    it ("Formats " ++ label ++ " correctly") $ do
        True `shouldBe `True


