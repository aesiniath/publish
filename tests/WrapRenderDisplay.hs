{-# LANGUAGE OverloadedStrings #-}

module WrapRenderDisplay where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified RenderDocument as Render (render)

main :: IO ()
main = do
    let args = ["Junk.list"]

    Render.render args

    input <- T.readFile "Junk.latex"
    T.putStrLn input
