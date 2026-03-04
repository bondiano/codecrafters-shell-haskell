module Shell.Env (
    Env (..),
    Shell,
    buildEnv,
) where

import Control.Monad.Reader (ReaderT)
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath)

data Env = Env {envPaths :: [FilePath], homeDir :: FilePath}

type Shell = ReaderT Env IO

buildEnv :: IO Env
buildEnv = do
    paths <- buildEnvPath
    home <- getHomeDirectory
    return Env{envPaths = paths, homeDir = home}

buildEnvPath :: IO [FilePath]
buildEnvPath = maybe [] splitSearchPath <$> lookupEnv "PATH"
