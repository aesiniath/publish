{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RenderDocument
    ( program
    , initial
    )
where

import Control.Monad (filterM, when)
import Core.Program
import Core.System
import Core.Text
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (doesFileExist, doesDirectoryExist
    , getModificationTime, copyFileWithMetadata)
import System.Exit (ExitCode(..))
import System.FilePath.Posix (takeBaseName, takeFileName, takeExtension)
import System.IO (openFile, IOMode(WriteMode), hClose)
import System.IO.Error (userError, IOError)
import System.Posix.Temp (mkdtemp)
import System.Process.Typed (proc, readProcess, setStdin, closed)
import Text.Pandoc (runIOorExplode, readMarkdown, writeLaTeX, def
    , readerExtensions, pandocExtensions, writerTopLevelDivision
    , TopLevelDivision(TopLevelChapter))

import LatexPreamble (preamble, ending)
import OutputParser (parseOutputForError)

data Env = Env
    { targetHandleFrom :: Handle
    , targetFilenameFrom :: FilePath
    , resultFilenameFrom :: FilePath
    , tempDirectoryFrom :: FilePath
    }

initial :: Env
initial = Env undefined "" "" ""

program :: Program Env ()
program = do
    bookfile <- extractBookFile

    event "Reading bookfile"
    files <- processBookFile bookfile

    event "Setup temporary directory"
    setupTargetFile bookfile

    event "Convert document fragments to LaTeX"
    mapM_ processFragment files

    event "Write intermediate LaTeX file"
    produceResult

    event "Render document to PDF"
    renderPDF
    copyHere

    event "Complete"

extractBookFile :: Program Env FilePath
extractBookFile = do
    params <- getCommandLine
    case lookupArgument "bookfile" params of
        Nothing -> invalid
        Just bookfile -> return bookfile

setupTargetFile :: FilePath -> Program Env ()
setupTargetFile name = do
    tmpdir <- liftIO $ catch
        (do
            dir' <- readFile dotfile
            let dir = trim dir'
            probe <- doesDirectoryExist dir
            if probe
                then return dir
                else throw boom
        )
        (\(_ :: IOError) -> do
            dir <- mkdtemp "/tmp/publish-"
            writeFile dotfile (dir ++ "\n")
            return dir
        )
    debugS "tmpdir" tmpdir

    let target = tmpdir ++ "/" ++ base ++ ".tex"
        result = tmpdir ++ "/" ++ base ++ ".pdf"

    handle <- liftIO (openFile target WriteMode)
    debugS "target" target

    params <- getCommandLine
    case lookupOptionFlag "default-preamble" params of
        Just True   -> liftIO $ do
            hWrite handle preamble
        _           -> return ()

    let env = Env
            { targetHandleFrom = handle
            , targetFilenameFrom = target
            , resultFilenameFrom = result
            , tempDirectoryFrom = tmpdir
            }
    setApplicationState env
  where
    dotfile = ".target"

    base = takeBaseName name -- "/directory/file.ext" -> "file"

    boom = userError "Temp dir no longer present"

    trim :: String -> String
    trim = dropWhileEnd isSpace


processBookFile :: FilePath -> Program Env [FilePath]
processBookFile file = do
    debugS "bookfile" file
    files <- liftIO $ do
        contents <- T.readFile file
        filterM doesFileExist (possibilities contents)

    return files
  where
    -- filter out blank lines and lines commented out
    possibilities :: Text -> [FilePath]
    possibilities = map T.unpack . filter (not . T.null)
        . filter (not . T.isPrefixOf "#") . T.lines

{-|
Which kind of file is it? Dispatch to the appropriate reader switching on
filename extension.
-}
processFragment :: FilePath -> Program Env ()
processFragment file = do
    debugS "fragment" file

    -- Read the fragment, process it if Markdown then run it out to LaTeX.
    case takeExtension file of
        ".markdown" -> convertMarkdown file
        ".latex"    -> passthroughLaTeX file
        ".svg"      -> generateImage file
        _           -> error "Unknown file extension"

{-|
Convert Markdown to LaTeX. This is where we "call" Pandoc.

Default behaviour from the command line is to activate all (?) of Pandoc's
Markdown extensions, but invoking via the `readMarkdown` function with
default ReaderOptions doesn't turn any on. Using `pandocExtensions` here
appears to represent the whole set.

When output format is LaTeX, the command-line _pandoc_ tool does some
somewhat convoluted heuristics to decide whether top-level headings (ie
<H1>, ====, #) are to be considered \part, \chapter, or \section.  The fact
that is not deterministic is annoying. Force the issue.

Finally, for some reason, the Markdown -> LaTeX pair strips trailing
whitespace from the block, resulting in a no paragraph boundary between
files. So gratuitously add a break.
-}
convertMarkdown :: FilePath -> Program Env ()
convertMarkdown file =
  let
    readingOptions = def { readerExtensions = pandocExtensions }

    writingOptions = def { writerTopLevelDivision = TopLevelChapter }

  in do
    env <- getApplicationState
    let handle = targetHandleFrom env

    liftIO $ do
        contents <- T.readFile file

        latex <- runIOorExplode $ do
            parsed <- readMarkdown readingOptions contents
            writeLaTeX writingOptions parsed

        T.hPutStrLn handle latex

        T.hPutStr handle "\n"


{-|
If a source fragment is already LaTeX, simply copy it through to
the target file.
-}
passthroughLaTeX :: FilePath -> Program Env ()
passthroughLaTeX file = do
    env <- getApplicationState
    let handle = targetHandleFrom env

    liftIO $ do
        contents <- T.readFile file
        T.hPutStrLn handle contents

generateImage :: FilePath -> Program Env ()
generateImage file =
  let
    output = takeBaseName file ++ ".pdf"
    rsvgConvert = proc "rsvg-convert"
        [ "--format=pdf"
        , "--output=" ++ output
        , file
        ]
  in do
    debugS "image" file
    debugS "output" output
    (exit,out,err) <- liftIO (readProcess (setStdin closed rsvgConvert))
    debugS "exitcode" exit
    case exit of
        ExitFailure _ ->  do
            event "Image processing failed"
            debug "stderr" (intoRope err)
            debug "stdout" (intoRope out)
            throw exit
        ExitSuccess -> return ()


{-|
Finish the intermediate target file.
-}
produceResult :: Program Env ()
produceResult = do
    env <- getApplicationState
    let handle = targetHandleFrom env

    params <- getCommandLine
    case lookupOptionFlag "default-preamble" params of
        Just True   -> liftIO $ do
            hWrite handle ending
            hClose handle
        _           -> liftIO $ do
            hClose handle


renderPDF :: Program Env ()
renderPDF = do
    env <- getApplicationState

    let target = targetFilenameFrom env
        result = resultFilenameFrom env
        tmpdir = tempDirectoryFrom env

        latexmk = proc "latexmk"

            [ "-xelatex"
            , "-output-directory=" ++ tmpdir
            , "-interaction=nonstopmode"
            , "-halt-on-error"
            , "-file-line-error"
            , target
            ]

    debugS "result" result
    (exit,out,err) <- liftIO (readProcess (setStdin closed latexmk))
    debugS "exitcode" exit
    case exit of
        ExitFailure _ ->  do
            event "Render failed"
            debug "stderr" (intoRope err)
            debug "stdout" (intoRope out)
            write (parseOutputForError target out)
            throw exit
        ExitSuccess -> return ()

copyHere :: Program Env ()
copyHere = do
    env <- getApplicationState
    let result = resultFilenameFrom env
        final = takeFileName result             -- ie ./Book.pdf
    withContext $ \runProgram -> do
        time1 <- getModificationTime result
        exists <- doesFileExist final
        time2 <- if exists
            then getModificationTime final
            else getModificationTime "/proc"    -- boot time!
        when (time1 > time2) $ do
            runProgram $ do
                event "Copy resultant PDF here"
                debugS "final" final
            copyFileWithMetadata result final
