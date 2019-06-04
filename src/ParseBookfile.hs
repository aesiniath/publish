module ParseBookfile where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

-- use String, since we need FilePaths which are type aliases over String anyway.
type Parser = Parsec Void String

-- ParseErrorBundle Text Void

data Bookfile = Bookfile
    { bookfileVersion :: Int
    , bookfilePreambles :: [FilePath]
    , bookfileFragments :: [FilePath]
    } deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

parseFileLine :: Parser FilePath
parseFileLine = do
    notFollowedBy (char '%')
    file <- takeWhile1P (Just "line containing a filename") (/= '\n')
    return file

__VERSION__ = 0

parseMagicLine :: Parser Int
parseMagicLine = do
    void (char '%') <?> "first line to begin with % character"
    void spaceChar <?> "a space character"
    void (string "publish")
    void spaceChar <?> "a space character"
    void (char 'v') <?> "the character v and then a number"
    v <- L.decimal <?> "the bookfile schema version number"
    unless (v == __VERSION__) (fail ("currently recognized bookfile schema version is v" ++ show __VERSION__))
    return v
    
--  void (many newline) 

parseBeginLine :: Parser ()
parseBeginLine = dbg "begin" $ try $ label "begin marker" $ do
    void (string "% begin")
    void newline
    return ()

parseEndLine :: Parser ()
parseEndLine = dbg "endin" $ try $ label "end marker" $ do
    void (string "% end")
    void newline

parseBlank :: Parser ()
parseBlank = do
    void (hidden (many newline))


-- HERE maybe something like
--
-- parseBlank <|> parseBegin <|> ...
--
-- in any event, the problem is parseFileLine is being overzealous (or is
-- insufficiently restricted)
--
parseBookfile :: Parser Bookfile
parseBookfile = do
    version <- parseMagicLine
    preambles <- many (parseBlank *> parseFileLine <* parseBlank)
    parseBeginLine
    fragments <- many (parseBlank *> parseFileLine <* parseBlank)
    parseEndLine
    return Bookfile
        { bookfileVersion = version
        , bookfilePreambles = preambles
        , bookfileFragments = fragments
        }
