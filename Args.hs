{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
module Args where

import Paths_activehs

import System.Directory (createDirectoryIfMissing)
import System.Console.CmdArgs.Implicit
import System.FilePath

import Data.Version

------------------

data Args
    = Args
        { sourcedir     :: String
        , includedir    :: [String]
        , gendir        :: String
        , exercisedir   :: String
        , templatedir   :: String
        , staticdir     :: String
        , logdir        :: String
        , publicdir     :: String

        , hoogledb      :: (Maybe String)

        , mainpage      :: String
        , restartpath   :: String

        , port          :: Int
        , lang          :: String
        , static        :: Bool
        , verbose       :: Int
        , verboseinterpreter :: Bool
        , recompilecmd  :: String
        , magicname     :: String
        , daemon        :: Bool
        , reloadsPerGhciSession :: Int
        }
        deriving (Show, Data, Typeable)

myargs :: FilePath -> Args
myargs dataDir = Args
        { sourcedir     = "."     &= typDir         &= help "Directory of lhs files to serve. Default is '.'"
        , includedir    = []      &= typDir         &= help "Additional include directory. You can specify more than one. Empty by default."
        , gendir        = "html"  &= typDir         &= help "Directory to put generated content to serve. Default is 'html'"
        , exercisedir   = "exercise" &= typDir      &= help "Directory to put generated exercises to serve. Default is 'exercise'"
        , templatedir   = (dataDir </> "template")      &= typDir         &= help "Directory of html template files for pandoc. Default points to the distribution's directory."
        , staticdir  = (dataDir </> "copy")      &= typDir         &= help "Files in this directory will be served as they are (for css and javascript files). Default points to the distribution's directory."
        , logdir        = "log"   &= typDir         &= help "Directory to put log files in. Default is 'log'."
        , publicdir     = ""      &= typDir         &= help "Directory of public (not css or javascript) files. Files in this directory will be served as-is."

        , hoogledb      = Nothing      &= typFile        &= help "Hoogle database file"

        , mainpage      = "Index.xml" &= typ "PATH" &= help "Main web page path"
        , restartpath   = ""      &= typ "PATH"     &= help "Opening this path in browser restarts the ghci server."

        , lang          = "en"    &= typ "LANGUAGE" &= help "Default language. It is 'en' by default."
        , port          = 8000    &= typ "PORT"     &= help "Port to listen"
        , static        = False                     &= help "Do not regenerate pages."
        , verbose       = 2                         &= help "Verbose activehs output"
        , verboseinterpreter = False                &= help "Verbose interpreter output in the browser"
        , recompilecmd  = "ghc -O -dynamic" &= typ "COMMAND" &= help "Command to run before page generation. Default is 'ghc -O'."
        , magicname    = "a9xYf"  &= typ "VARNAME"  &= help "Magic variable name."
        , daemon       = False                      &= help "Run as a service."
        , reloadsPerGhciSession = 15                        &= help "Number of file reloads per ghci restarts."
        }  &= summary ("activehs " ++ showVersion version ++ ", (C) 2010-2012 Péter Diviánszky")
           &= program "activehs"


createDirs :: Args -> IO ()
createDirs (Args {sourcedir, gendir, exercisedir, logdir}) 
    = mapM_ f [sourcedir, gendir, exercisedir, logdir]
 where
    f "" = return ()
    f path = createDirectoryIfMissing True path


getArgs :: IO Args
getArgs = do
    dataDir <- getDataDir
    args <- cmdArgs (myargs dataDir)
    createDirs args
    return args



