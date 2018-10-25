{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RenderDocument
    ( program
    , initial
    )
where

import Control.Monad (filterM)
import Core.Program
import Core.System
import Core.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (doesFileExist)
import System.FilePath.Posix (takeBaseName)
import System.IO (openBinaryFile, IOMode(WriteMode), hClose)
import System.Posix.Temp (mkdtemp)
import System.Process.Typed (proc, runProcess_, setStdin, closed)
import Text.Pandoc

data Env = Env
    { targetHandleFrom :: Handle
    , targetFilenameFrom :: FilePath
    , resultFilenameFrom :: FilePath
    , tempDirectoryFrom :: FilePath
    }

initial :: Env
initial = Env stdout "" "" ""

program :: Program Env ()
program = do
    bookfile <- extractBookFile

    event "Reading bookfile"
    files <- processBookFile bookfile

    event "Setup intermediate target"
    setupTargetFile bookfile

    event "Converting pieces"
    mapM_ processFragment files

    event "Write intermediate"
    produceResult

    event "Render document"
    renderPDF

    event "Complete"

extractBookFile :: Program Env FilePath
extractBookFile = do
    params <- getCommandLine
    case lookupArgument "bookfile" params of
        Nothing -> invalid
        Just bookfile -> return bookfile

setupTargetFile :: FilePath -> Program Env ()
setupTargetFile name = do
    tmpdir <- temporaryBuildDir

    let target = tmpdir ++ "/" ++ base ++ ".latex"
        result = tmpdir ++ "/" ++ base ++ ".pdf"

    handle <- liftIO (openBinaryFile target WriteMode)

    liftIO $ hWrite handle [quote|
\documentclass[12pt,a4paper,openany]{memoir}

%
% Load the TeX Gyre project's "Heros" font, which is an upgrade of URW's
% lovely "Nimbus Sans L" sans-serif font.
%

\usepackage{fontspec}
\setmainfont{Linux Libertine O}
\setsansfont{TeX Gyre Heros}[Scale=MatchLowercase]
\setmonofont{Inconsolata}[Scale=MatchLowercase]

%\usepackage[showframe, pass]{geometry}

% use upquote for straight quotes in verbatim environments
\usepackage{upquote}

% use microtype
\usepackage{microtype}
\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts

%
% Customize paper size. Or not: A4 paper is 597pt x 845pt. 4:3 aka 768x1024
% screen is 597pt x 796pt, but 16:9 aka 2560x1440 screen is 597pt x 1062pt. A4
% in landscape is a fair way narrower.
%

\setlrmarginsandblock{2cm}{2.5cm}{*}
\setulmarginsandblock{2cm}{2cm}{*}

%
% Setting the \footskip parameter is how you control the bottom margin width,
% not "setting the bottom margin" since the typeblock will be set to be an
% integer multiple of \baselineskip.
%

\setheadfoot{0pt}{25pt}
\setheaderspaces{1cm}{*}{*}

\checkandfixthelayout[classic]

\usepackage{graphicx,grffile}

\usepackage{longtable}

\setlength{\emergencystretch}{3em}  % prevent overfull lines

\usepackage[hidelinks]{hyperref}

\SingleSpacing
\traditionalparskip
\setlength{\parindent}{0em}

%
% Customize the section heading fonts to use this accordingly.
%

\chapterstyle{article}
\setsecnumdepth{none}

% FIXME Why isn't the \Huge font size command working?
\renewcommand{\chaptitlefont}{\Large\sffamily\bfseries}

\setsecheadstyle{\large\sffamily}
\setsubsecheadstyle{\normalsize\sffamily\bfseries}
\setsubsubsecheadstyle{\normalsize\rmfamily\itshape}

\begin{document}
|]

    let env = Env
            { targetHandleFrom = handle
            , targetFilenameFrom = target
            , resultFilenameFrom = result
            , tempDirectoryFrom = tmpdir
            }
    setApplicationState env
  where
    base = takeBaseName name -- "/directory/file.ext" -> "file"

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

processFragment :: FilePath -> Program Env ()
processFragment file = do
    env <- getApplicationState
    let handle = targetHandleFrom env

    debugS "fragment" file
    liftIO $ do
        contents <- T.readFile file
        latex <- runIOorExplode $ do
            parsed <- readMarkdown def contents
            writeLaTeX def parsed

        T.hPutStrLn handle latex

        -- for some reason, the Markdown -> LaTeX pair strips trailing
        -- whitespace from the block, resulting in a no paragraph boundary
        -- between files. So gratuitously add a break
        T.hPutStr handle "\n"

temporaryBuildDir :: Program Env FilePath
temporaryBuildDir = do
    dirname <- liftIO $ mkdtemp "/tmp/publish-"
    debugS "tmpdir" dirname
    return dirname

-- finish file
produceResult :: Program Env ()
produceResult = do
    env <- getApplicationState
    let handle = targetHandleFrom env
    liftIO $ do
        hWrite handle [quote|
\end{document}
        |]
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
        copy = proc "cp"
            [ result
            , "."
            ]

    debugS "result" result
    liftIO $ do
        runProcess_ (setStdin closed latexmk)
        runProcess_ (setStdin closed copy)
