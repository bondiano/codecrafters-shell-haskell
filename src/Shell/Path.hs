module Shell.Path (
    getExecutableNames,
    getExecutablePathFromPaths,
    isExecutable,
) where

import Control.Monad (filterM)
import Control.Monad.ListM (findM)
import Data.List (nub)
import System.Directory (doesDirectoryExist, doesFileExist, executable, getPermissions, listDirectory)
import System.FilePath ((</>))

getExecutablePathFromPaths :: [FilePath] -> String -> IO (Maybe String)
getExecutablePathFromPaths dirs cmd =
    fmap (</> cmd) <$> findM (\d -> isExecutable (d </> cmd)) dirs

isExecutable :: FilePath -> IO Bool
isExecutable path = do
    exists <- doesFileExist path
    if exists then executable <$> getPermissions path else return False

getExecutableNames :: [FilePath] -> IO [String]
getExecutableNames dirs = nub . concat <$> mapM listExecutables dirs
  where
    listExecutables dir = do
        exists <- doesDirectoryExist dir
        if exists
            then do
                files <- listDirectory dir
                filterM (\f -> isExecutable (dir </> f)) files
            else pure []
