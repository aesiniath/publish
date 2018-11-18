module Environment
(
      Env(..)
    , initial
)
where

data Env = Env
    { intermediateFilenamesFrom :: [FilePath]
    , masterFilenameFrom :: FilePath
    , resultFilenameFrom :: FilePath
    , tempDirectoryFrom :: FilePath
    }

initial :: Env
initial = Env [] "" "" ""

