{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shell.Env (
    Env (..),
    Shell (..),
    buildEnv,
    addHistory,
    getHistory,
    getUnsavedHistory,
    markHistorySaved,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath)

data Env = Env {envPaths :: [FilePath], homeDir :: FilePath, historyRef :: IORef [String], lastSavedRef :: IORef Int}

newtype Shell a = Shell {runShell :: ReaderT Env IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

buildEnv :: IO Env
buildEnv = do
    paths <- buildEnvPath
    home <- getHomeDirectory
    histFile <- lookupEnv "HISTFILE"
    initialHist <- case histFile of
        Just path -> do
            exists <- doesFileExist path
            if exists
                then filter (not . null) . lines <$> readFile path
                else pure []
        Nothing -> pure []
    hist <- newIORef initialHist
    saved <- newIORef (length initialHist)
    return Env{envPaths = paths, homeDir = home, historyRef = hist, lastSavedRef = saved}

addHistory :: String -> Shell ()
addHistory entry = do
    ref <- asks historyRef
    liftIO $ modifyIORef' ref (++ [entry])

getHistory :: Shell [String]
getHistory = asks historyRef >>= liftIO . readIORef

getUnsavedHistory :: Shell [String]
getUnsavedHistory = do
    entries <- getHistory
    savedIdx <- asks lastSavedRef >>= liftIO . readIORef
    pure $ drop savedIdx entries

markHistorySaved :: Shell ()
markHistorySaved = do
    entries <- getHistory
    ref <- asks lastSavedRef
    liftIO $ writeIORef ref (length entries)

buildEnvPath :: IO [FilePath]
buildEnvPath = maybe [] splitSearchPath <$> lookupEnv "PATH"
