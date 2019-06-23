module ParseBookfile where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Environment (Bookfile(..))

-- use String, since we need FilePaths which are type aliases over String anyway.
type Parser = Parsec Void String

__VERSION__ :: Int
__VERSION__ = 2

parseMagicLine :: Parser Int
parseMagicLine = do
    void (char '%') <?> "first line to begin with % character"
    void spaceChar <?> "a space character"
    void (string "publish")
    void spaceChar <?> "a space character"
    void (char 'v') <?> "the character v and then a number"
    v <- L.decimal <?> "the bookfile schema version number"
    unless (v == __VERSION__) (fail ("currently recognized bookfile schema version is v" ++ show __VERSION__))
    void newline
    return v

parseBeginLine :: Parser ()
parseBeginLine = try $ label "begin marker" $ do
    void (string "% begin")
    void newline

parseFileLine :: Parser FilePath
parseFileLine = do
    notFollowedBy (char '%')
    file <- takeWhile1P (Just "line containing a filename") (/= '\n')
    return file

parseEndLine :: Parser ()
parseEndLine = try $ label "end marker" $ do
    void (string "% end")
    void newline

parseBlank :: Parser ()
parseBlank = do
    void (hidden (many newline))

parseBookfile :: Parser Bookfile
parseBookfile = do
    version <- parseMagicLine
    preambles <- many (parseBlank *> parseFileLine <* parseBlank)
    parseBeginLine
    fragments <- many (parseBlank *> parseFileLine <* parseBlank)
    parseEndLine
    trailers <- many (parseBlank *> parseFileLine <* parseBlank)
    return Bookfile
        { versionFrom = version
        , preamblesFrom = preambles
        , fragmentsFrom = fragments
        , trailersFrom = trailers
        }
