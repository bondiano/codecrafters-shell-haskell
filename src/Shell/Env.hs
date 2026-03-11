{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shell.Env (
    Env (..),
    Shell (..),
    buildEnv,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath)

data Env = Env {envPaths :: [FilePath], homeDir :: FilePath}

newtype Shell a = Shell {runShell :: ReaderT Env IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

buildEnv :: IO Env
buildEnv = do
    paths <- buildEnvPath
    home <- getHomeDirectory
    return Env{envPaths = paths, homeDir = home}

buildEnvPath :: IO [FilePath]
buildEnvPath = maybe [] splitSearchPath <$> lookupEnv "PATH"
