module Shell.Path (
    getExecutablePathFromPaths,
    isExecutable,
) where

import Control.Monad.ListM (findM)
import System.Directory (doesFileExist, executable, getPermissions)
import System.FilePath ((</>))

getExecutablePathFromPaths :: [FilePath] -> String -> IO (Maybe String)
getExecutablePathFromPaths dirs cmd =
    fmap (</> cmd) <$> findM (\d -> isExecutable (d </> cmd)) dirs

isExecutable :: FilePath -> IO Bool
isExecutable path = do
    exists <- doesFileExist path
    if exists then executable <$> getPermissions path else return False
