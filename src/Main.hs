import Data.Maybe
import Data.Char
import Data.Monoid
import System.Environment
import System.Directory
import System.FilePath.Posix

main = do
    arg <- getArgs
    case arg of
         [] -> help NoArgumentsGiven
         (ftype:_) -> mkIgn ftype
defaultDir :: IO FilePath
defaultDir = do
    home <- getHomeDirectory
    return $ home </> ".ignFiles/"

mkIgn :: String -> IO ()
mkIgn ftype = do
    f <- getIgnFilePath ftype
    case f of
         Left err -> help err -- TODO : handle FileNotFound
         Right fPath -> cpIgn fPath

getIgnFilePath :: String -> IO (Either Err FilePath)
getIgnFilePath ftype = do
    g <- getIgnDir
    case g of
         (Right dir) -> searchIgnFile ftype dir
         (Left _) -> return g

searchIgnFile :: String -> FilePath -> IO (Either Err FilePath)
searchIgnFile ftype' dir = do
    let ftype = headCapt . normaliseFileType $ ftype'
        fname = ftype <> ".gitignore"
        loc = dir </> fname
    b <- doesFileExist loc
    if b then return (Right loc)
         else return (Left FileNotFound)

normaliseFileType :: String -> String
normaliseFileType "hs" = "haskell"
normaliseFileType "hst" = "haste"
normaliseFileType x = x
headCapt :: String -> String
headCapt [] = []
headCapt (h:t) = toUpper h : t

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
         | FileNotFound
         | NoArgumentsGiven

help :: Err -> IO ()
help NoArgumentsGiven = putStr .  unlines $
    [
        "ign : .gitignore generator",
        "usage : ign [filetype]"
    ]
help DirNotFound = putStr .  unlines $
    [
        "directry not found.  try `$ export IGN_DIR=/path/to/dir`"
    ]
help FileNotFound = putStr .  unlines $
    [
        "file not found"
    ]
