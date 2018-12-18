{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import CompareFragments

main :: IO ()
main = do
    hspec suite
    putStrLn "."

suite :: Spec
suite = do
    checkByComparingFragments
