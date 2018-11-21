module Environment
(
      Env(..)
    , initial
)
where

import System.Posix.Directory (getWorkingDirectory)

data Env = Env
    { startingDirectoryFrom :: FilePath
    , basenameFrom :: String
    , intermediateFilenamesFrom :: [FilePath]
    , masterFilenameFrom :: FilePath
    , resultFilenameFrom :: FilePath
    , tempDirectoryFrom :: FilePath
    }

initial :: IO Env
initial = do
    cwd <- getWorkingDirectory
    return (Env cwd "None" [] "/dev/null" "/dev/null" "/dev/null")

