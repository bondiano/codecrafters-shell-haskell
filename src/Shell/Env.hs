{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shell.Env (
    Env (..),
    Shell (..),
    buildEnv,
    addHistory,
    getHistory,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath)

data Env = Env {envPaths :: [FilePath], homeDir :: FilePath, historyRef :: IORef [String]}

newtype Shell a = Shell {runShell :: ReaderT Env IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

buildEnv :: IO Env
buildEnv = do
    paths <- buildEnvPath
    home <- getHomeDirectory
    hist <- newIORef []
    return Env{envPaths = paths, homeDir = home, historyRef = hist}

addHistory :: String -> Shell ()
addHistory entry = do
    ref <- asks historyRef
    liftIO $ modifyIORef' ref (++ [entry])

getHistory :: Shell [String]
getHistory = asks historyRef >>= liftIO . readIORef

buildEnvPath :: IO [FilePath]
buildEnvPath = maybe [] splitSearchPath <$> lookupEnv "PATH"
