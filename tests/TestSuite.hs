{-# LANGUAGE OverloadedStrings #-}

import CheckBookfileParser
import CheckTableProperties
import CompareFragments
import Core.System
import Test.Hspec

main :: IO ()
main = do
  finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
  checkTableProperties
  checkByComparingFragments
  checkBookfileParser
