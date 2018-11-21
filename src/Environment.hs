module Environment
(
      Env(..)
    , initial
)
where

import System.Posix.Directory (getWorkingDirectory)

data Env = Env
    { startingDirectoryFrom :: FilePath
    , intermediateFilenamesFrom :: [FilePath]
    , masterFilenameFrom :: FilePath
    , resultFilenameFrom :: FilePath
    , tempDirectoryFrom :: FilePath
    }

initial :: IO Env
initial = do
    cwd <- getWorkingDirectory
    return (Env cwd [] "/dev/null" "/dev/null" "/dev/null")

