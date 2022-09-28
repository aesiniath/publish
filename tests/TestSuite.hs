{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import CheckBookfileParser
import CheckTableProperties
import CompareFragments
import Control.Exception.Safe qualified as Safe
import Test.Hspec

main :: IO ()
main = do
    Safe.finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkTableProperties
    checkByComparingFragments
    checkBookfileParser
