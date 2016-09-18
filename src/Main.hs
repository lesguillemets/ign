module Main (main) where

import Data.Maybe
import Data.Monoid
import Prelude hiding (writeFile)
import Data.ByteString.Lazy.Char8 (writeFile)
import System.Environment
import System.Directory
import System.FilePath.Posix

import Base
import Download

main = do
    arg <- getArgs
    case arg of
         [] -> help NoArgumentsGiven
         (fType:_) -> mkIgn fType
defaultDir :: IO FilePath
defaultDir = do
    home <- getHomeDirectory
    return $ home </> ".ignFiles/"

mkIgn :: String -> IO ()
mkIgn fType = do
    f <- getIgnFilePath fType
    case f of
         Left err -> help err -- TODO : handle FileNotFound
         Right fPath -> cpIgn fPath

getIgnFilePath :: String -> IO (Either Err FilePath)
getIgnFilePath fType = do
    g <- getIgnDir
    case g of
         (Right dir) -> searchIgnFile fType dir
         (Left _) -> return g

searchIgnFile :: String -> FilePath -> IO (Either Err FilePath)
searchIgnFile fType' dir = do
    let fType = normaliseFileType $ fType'
        fname = fType <> ".gitignore"
        loc = dir </> fname
    b <- doesFileExist loc
    if b then return (Right loc)
         else return (Left (FileNotFound fname loc))

getIgnDir :: IO (Either Err FilePath)
getIgnDir = do
    def <- defaultDir
    dir <- fromMaybe def <$> lookupEnv "IGN_DIR"
    exists <- doesDirectoryExist dir
    if exists
       then return (Right dir)
       else return (Left DirNotFound)

cpIgn :: FilePath -> IO ()
cpIgn f = do
    d <- getCurrentDirectory
    let loc = d </> ".gitignore"
    b <- doesFileExist loc
    if b then putStrLn ".gitignore already exists. abort."
        else copyFile f loc

data Err = DirNotFound
         | FileNotFound String FilePath
         | NoArgumentsGiven

help :: Err -> IO ()
help NoArgumentsGiven = putStr .  unlines $
    [
        "ign : .gitignore generator",
        "usage : ign [filetype]"
    ]
help DirNotFound = putStr .  unlines $
    [
        "directry not found.  try `$ export IGN_DIR=/path/to/dir`.",
        "By default, `~/.ignFiles/` is used."
    ]
help (FileNotFound f path) = do
    putStrLn "File not found. I'll try to download from github.."
    ignore <- ignDownload f
    case ignore of
         Nothing -> putStrLn "Still not found. Invalid filetype?"
         Just x -> do
             putStrLn $ "Found it. Saving to " <> path
             writeFile path x
             cpIgn path
