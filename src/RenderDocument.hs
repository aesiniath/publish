{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RenderDocument (
    program,
) where

import Control.Monad (filterM, forM_, forever, void)
import Core.Data
import Core.Program
import Core.System
import Core.Telemetry
import Core.Text
import Data.Char (isSpace)
import qualified Data.List as List (dropWhileEnd, null)
import Data.Maybe (isJust)
import qualified Data.Text.IO as T
import Environment (Bookfile (..), Env (..))
import LatexOutputReader (parseOutputForError)
import LatexPreamble (beginning, ending, preamble)
import ParseBookfile (parseBookfile)
import System.Directory (
    copyFileWithMetadata,
    doesDirectoryExist,
    doesFileExist,
    renameFile,
 )
import System.Exit (ExitCode (..))
import System.FilePath.Posix (
    dropExtension,
    replaceDirectory,
    replaceExtension,
    splitFileName,
    takeBaseName,
    takeExtension,
 )
import System.IO (hPutStrLn)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Temp (mkdtemp)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.Pandoc (
    TopLevelDivision (TopLevelSection),
    def,
    pandocExtensions,
    readMarkdown,
    readerColumns,
    readerExtensions,
    runIOorExplode,
    writeLaTeX,
    writerTopLevelDivision,
 )
import Utilities (ensureDirectory, ifNewer, isNewer)

data Mode = Once | Cycle

data Copy = InstallPdf | NoCopyPdf

program :: Program Env ()
program = do
    params <- getCommandLine
    (mode, copy) <- extractMode params

    info "Identify .book file"
    bookfile <- extractBookFile params

    case mode of
        Once -> do
            -- normal operation, single pass
            void (renderDocument (mode, copy) bookfile)
        Cycle -> do
            -- use inotify to rebuild on changes
            forever (renderDocument (mode, copy) bookfile >>= waitForChange >> resetTimer)

renderDocument :: (Mode, Copy) -> FilePath -> Program Env [FilePath]
renderDocument (mode, copy) file = do
    setServiceName "render"
    beginTrace $ do
        encloseSpan "Render document" $ do
            telemetry
                [ metric "bookfile" file
                ]

            book <- encloseSpan "Setup" $ do
                info "Read .book file"
                book <- processBookFile file

                info "Setup temporary directory"
                setupTargetFile file
                setupPreambleFile
                validatePreamble book

                pure book

            let preambles = preamblesFrom book
            let fragments = fragmentsFrom book
            let trailers = trailersFrom book

            encloseSpan "Convert fragments" $ do
                info "Convert preamble fragments and begin marker to LaTeX"
                mapM_ processFragment preambles
                setupBeginningFile

                info "Convert document fragments to LaTeX"
                mapM_ processFragment fragments

                info "Convert end marker and trailing fragments to LaTeX"
                setupEndingFile
                mapM_ processFragment trailers

                info "Write intermediate LaTeX file"
                produceResult

            encloseSpan "Render LaTeX to PDF" $ do
                info "Render document to PDF"
                catch
                    ( do
                        renderPDF
                        case copy of
                            InstallPdf -> copyHere
                            NoCopyPdf -> return ()
                    )
                    ( \(e :: ExitCode) -> case mode of
                        Once -> throw e
                        Cycle -> return ()
                    )

            pure (uniqueList file preambles fragments trailers)

--
-- Quickly reduce the fragment names to a unique list so we don't waste
-- inotify watches.
--
uniqueList :: FilePath -> [FilePath] -> [FilePath] -> [FilePath] -> [FilePath]
uniqueList file preambles fragments trailers =
    let files = insertElement file (intoSet trailers <> (intoSet preambles <> intoSet fragments))
     in fromSet files

extractMode :: Parameters -> Program Env (Mode, Copy)
extractMode params =
    let mode = case lookupOptionFlag "watch" params of
            Just False -> error "Invalid State"
            Just True -> Cycle
            Nothing -> Once
        copy = case lookupOptionFlag "no-copy" params of
            Just False -> error "Invalid State"
            Just True -> NoCopyPdf
            Nothing -> InstallPdf
     in return (mode, copy)

{-
For the situation where the .book file is in a location other than '.'
then chdir there first, so any relative paths within _it_ are handled
properly, as are inotify watches later if they are employed.
-}
extractBookFile :: Parameters -> Program Env FilePath
extractBookFile params =
    let (relative, bookfile) = case lookupArgument "bookfile" params of
            Nothing -> error "invalid"
            Just file -> splitFileName file
     in do
            debugS "relative" relative
            debugS "bookfile" bookfile
            probe <- liftIO $ do
                changeWorkingDirectory relative
                doesFileExist bookfile
            case probe of
                True -> return bookfile
                False -> do
                    write ("error: specified .book file \"" <> intoRope bookfile <> "\" not found.")
                    throw (userError "no such file")

setupTargetFile :: FilePath -> Program Env ()
setupTargetFile file = do
    env <- getApplicationState
    let start = startingDirectoryFrom env
    let dotfile = start ++ "/.target"

    params <- getCommandLine
    tmpdir <- case lookupOptionValue "temp" params of
        Just dir -> do
            -- Append a slash so that /tmp/booga is taken as a directory.
            -- Otherwise, you end up ensuring /tmp exists.
            ensureDirectory (dir ++ "/")
            return dir
        Nothing ->
            liftIO $
                catch
                    ( do
                        dir' <- readFile dotfile
                        let dir = trim dir'
                        probe <- doesDirectoryExist dir
                        if probe
                            then return dir
                            else throw boom
                    )
                    ( \(_ :: IOError) -> do
                        dir <- mkdtemp "/tmp/publish-"
                        writeFile dotfile (dir ++ "\n")
                        return dir
                    )
    debugS "tmpdir" tmpdir

    let master = tmpdir ++ "/" ++ base ++ ".tex"
        result = tmpdir ++ "/" ++ base ++ ".pdf"

    let env' =
            env
                { intermediateFilenamesFrom = []
                , masterFilenameFrom = master
                , resultFilenameFrom = result
                , tempDirectoryFrom = tmpdir
                }
    setApplicationState env'
  where
    base = takeBaseName file -- "/directory/file.ext" -> "file"
    boom = userError "Temp dir no longer present"
    trim :: String -> String
    trim = List.dropWhileEnd isSpace

setupPreambleFile :: Program Env ()
setupPreambleFile = do
    env <- getApplicationState
    let tmpdir = tempDirectoryFrom env

    params <- getCommandLine
    first <- case lookupOptionFlag "builtin-preamble" params of
        Nothing -> return []
        Just True -> do
            let name = "00_Preamble.latex"
            let target = tmpdir ++ "/" ++ name
            liftIO $
                withFile target WriteMode $ \handle -> do
                    hWrite handle preamble
            return [name]
        Just _ -> invalid

    let env' = env{intermediateFilenamesFrom = first}
    setApplicationState env'

{-
This could do a lot more; checking to see if \documentclass is present, for
example. At present this covers the (likely common) failure mode of
specifying neither -p nor a preamble in the bookfile.
-}
validatePreamble :: Bookfile -> Program Env ()
validatePreamble book = do
    params <- getCommandLine
    let preambles = preamblesFrom book
    let builtin = isJust (lookupOptionFlag "builtin-preamble" params)

    if List.null preambles && not builtin
        then do
            write "error: no preamble\n"
            let msg :: Rope =
                    [quote|
You need to either a) put the name of the file including the LaTeX
preamble for your document in the .book file between the "% publish"
and "% begin" lines, or b) specify the --builtin-preamble option on
the command-line when running this program.
|]
            writeR msg
            terminate 2
        else return ()

setupBeginningFile :: Program Env ()
setupBeginningFile = do
    env <- getApplicationState
    let tmpdir = tempDirectoryFrom env
        files = intermediateFilenamesFrom env

    file <- do
        let name = "99_Beginning.latex"
        let target = tmpdir ++ "/" ++ name
        liftIO $
            withFile target WriteMode $ \handle -> do
                hWrite handle beginning
        return name

    let env' = env{intermediateFilenamesFrom = file : files}
    setApplicationState env'

setupEndingFile :: Program Env ()
setupEndingFile = do
    env <- getApplicationState
    let tmpdir = tempDirectoryFrom env
        files = intermediateFilenamesFrom env

    file <- do
        let name = "ZZ_Ending.latex"
        let target = tmpdir ++ "/" ++ name
        liftIO $
            withFile target WriteMode $ \handle -> do
                hWrite handle ending
        return name

    let env' = env{intermediateFilenamesFrom = file : files}
    setApplicationState env'

processBookFile :: FilePath -> Program Env Bookfile
processBookFile file = do
    contents <- liftIO (readFile file)

    let result = runParser parseBookfile file contents
    bookfile <- case result of
        Left err -> do
            write (intoRope (errorBundlePretty err))
            terminate 1
        Right value -> return value

    list1 <- filterM skipNotFound (preamblesFrom bookfile)
    debugS "preambles" (length list1)

    list2 <- filterM skipNotFound (fragmentsFrom bookfile)
    debugS "fragments" (length list2)

    list3 <- filterM skipNotFound (trailersFrom bookfile)
    debugS "trailers" (length list3)

    return bookfile{preamblesFrom = list1, fragmentsFrom = list2, trailersFrom = list3}
  where
    skipNotFound :: FilePath -> Program t Bool
    skipNotFound fragment = do
        probe <- liftIO (doesFileExist fragment)
        case probe of
            True -> return True
            False -> do
                warn "Fragment not found"
                write ("warning: Fragment \"" <> intoRope fragment <> "\" not found, skipping")
                return False

{-
Which kind of file is it? Dispatch to the appropriate reader switching on
filename extension.
-}
processFragment :: FilePath -> Program Env ()
processFragment file = do
    debugS "source" file

    -- Read the fragment, process it if Markdown then run it out to LaTeX.
    case takeExtension file of
        ".markdown" -> convertMarkdown file
        ".md" -> convertMarkdown file
        ".latex" -> passthroughLaTeX file
        ".tex" -> passthroughLaTeX file
        ".svg" -> convertImage file
        _ -> passthroughImage file

{-
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
    let readingOptions =
            def
                { readerExtensions = pandocExtensions
                , readerColumns = 75
                }
        writingOptions =
            def
                { writerTopLevelDivision = TopLevelSection
                }
     in do
            encloseSpan "convertMarkdown" $ do
                env <- getApplicationState
                let tmpdir = tempDirectoryFrom env
                    file' = replaceExtension file ".latex"
                    target = tmpdir ++ "/" ++ file'
                    files = intermediateFilenamesFrom env

                ensureDirectory target
                ifNewer file target $ do
                    debugS "target" target
                    liftIO $ do
                        contents <- T.readFile file

                        latex <- runIOorExplode $ do
                            parsed <- readMarkdown readingOptions contents
                            writeLaTeX writingOptions parsed

                        withFile target WriteMode $ \handle -> do
                            T.hPutStrLn handle latex
                            T.hPutStr handle "\n"

                let env' = env{intermediateFilenamesFrom = file' : files}
                setApplicationState env'

                telemetry
                    [ metric "file" file
                    ]

{-
If a source fragment is already LaTeX, simply copy it through to
the target file.
-}
passthroughLaTeX :: FilePath -> Program Env ()
passthroughLaTeX file = do
    encloseSpan "passthroughLaTeX" $ do
        env <- getApplicationState
        let tmpdir = tempDirectoryFrom env
            target = tmpdir ++ "/" ++ file
            files = intermediateFilenamesFrom env

        ensureDirectory target
        ifNewer file target $ do
            debugS "target" target
            liftIO $ do
                copyFileWithMetadata file target

        let env' = env{intermediateFilenamesFrom = file : files}
        setApplicationState env'
        telemetry
            [ metric "file" file
            ]

{-
Images in SVG format need to be converted to PDF to be able to be
included in the output as LaTeX doesn't understand SVG natively, which
is slightly shocking.
-}
convertImage :: FilePath -> Program Env ()
convertImage file = do
    encloseSpan "convertImage" $ do
        telemetry
            [ metric "file" file
            ]
        env <- getApplicationState
        let tmpdir = tempDirectoryFrom env
            basepath = dropExtension file
            target = tmpdir ++ "/" ++ basepath ++ ".pdf"
            buffer = tmpdir ++ "/" ++ basepath ++ "~tmp.pdf"
            inkscape =
                [ "inkscape"
                , "--export-type=pdf"
                , "--export-filename=" ++ buffer
                , file
                ]

        ifNewer file target $ do
            debugS "target" target
            (exit, out, err) <- do
                ensureDirectory target
                execProcess (fmap intoRope inkscape)

            case exit of
                ExitFailure _ -> do
                    info "Image processing failed"
                    debug "stderr" (intoRope err)
                    debug "stdout" (intoRope out)
                    write ("error: Unable to convert " <> intoRope file <> " from SVG to PDF")
                    throw exit
                ExitSuccess -> liftIO $ do
                    renameFile buffer target

passthroughImage :: FilePath -> Program Env ()
passthroughImage file = do
    encloseSpan "passthroughImage" $ do
        telemetry
            [ metric "file" file
            ]
        env <- getApplicationState
        let tmpdir = tempDirectoryFrom env
            target = tmpdir ++ "/" ++ file

        ensureDirectory target
        ifNewer file target $ do
            debugS "target" target
            liftIO $ do
                copyFileWithMetadata file target

{-
Finish up by writing the intermediate "master" file.
-}
produceResult :: Program Env ()
produceResult = do
    env <- getApplicationState
    let master = masterFilenameFrom env
        files = intermediateFilenamesFrom env

    debugS "master" master
    liftIO $
        withFile master WriteMode $ \handle -> do
            hPutStrLn handle ("\\RequirePackage{import}")
            forM_ (reverse files) $ \file -> do
                let (path, name) = splitFileName file
                hPutStrLn handle ("\\subimport{" ++ path ++ "}{" ++ name ++ "}")

getUserID :: Program a String
getUserID = liftIO $ do
    uid <- getEffectiveUserID
    gid <- getEffectiveGroupID
    return (show uid ++ ":" ++ show gid)

renderPDF :: Program Env ()
renderPDF = do
    env <- getApplicationState

    let master = masterFilenameFrom env
        tmpdir = tempDirectoryFrom env

    user <- getUserID

    params <- getCommandLine
    let command = case lookupOptionValue "docker" params of
            Just image ->
                [ "docker"
                , "run"
                , "--rm=true"
                , "--volume=" ++ tmpdir ++ ":" ++ tmpdir
                , "--user=" ++ user
                , image
                , "latexmk"
                ]
            Nothing ->
                [ "latexmk"
                ]
        options =
            [ "-lualatex"
            , "-output-directory=" ++ tmpdir
            , "-interaction=nonstopmode"
            , "-halt-on-error"
            , "-file-line-error"
            , "-cd"
            , master
            ]
        latexmk = command ++ options

    (exit, out, err) <- execProcess (fmap intoRope latexmk)
    case exit of
        ExitFailure _ -> do
            info "Render failed"
            debug "stderr" (intoRope err)
            debug "stdout" (intoRope out)
            write (parseOutputForError tmpdir out)
            throw exit
        ExitSuccess -> return ()

copyHere :: Program Env ()
copyHere = do
    env <- getApplicationState
    let result = resultFilenameFrom env
        start = startingDirectoryFrom env
        final = replaceDirectory result start -- ie ./Book.pdf
    changed <- isNewer result final
    case changed of
        True -> do
            info "Copy resultant PDF to starting directory"
            debugS "result" result
            debugS "final" final
            liftIO $ do
                copyFileWithMetadata result final
            info "Complete"
        False -> do
            info "Result unchanged"

    telemetry
        [ metric "changed" changed
        ]
