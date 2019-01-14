{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Core.System
import CompareFragments

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkByComparingFragments
