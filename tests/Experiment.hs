{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text.IO as T
import Text.Pandoc
import Text.Pandoc.Walk (walk)

behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x = x

readDoc :: PandocMonad m => Text -> m Pandoc
readDoc s = readMarkdown def s 

writeDoc :: PandocMonad m => Pandoc -> m Text
writeDoc doc = writeMarkdown def doc



main :: IO ()
main = do
    input <- T.readFile "examples/Headings.markdown"
    result <- runIOorExplode (process input)
    T.putStrLn result

process :: Text -> PandocIO Text
process input = do
    doc <- readDoc input
    let doc' = walk behead doc
    writeDoc doc'
