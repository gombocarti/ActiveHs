{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
module ActiveHs.Args where

import Paths_activehs

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing)
import System.Console.CmdArgs.Implicit
import System.FilePath

import ActiveHs.Translation.Base (Language(En))

import Data.Version

------------------

data Args
    = Args
        { sourceDir     :: FilePath
        , genDir        :: FilePath
        , exerciseDir   :: FilePath
        , logDir        :: FilePath
        , staticDir     :: FilePath

        , hoogledb      :: (Maybe FilePath)

        , mainpage      :: FilePath

        , port          :: Int
        , lang          :: Language
        , recompilecmd  :: String
        , magicname     :: String
        , daemon        :: Bool
        }
        deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args
        { sourceDir     = "."     &= typDir         &= help "Directory of lhs files to serve. Default is '.'"
        , genDir        = "html"  &= typDir         &= help "Directory to put generated content to serve. Default is 'html'"
        , exerciseDir   = "exercise" &= typDir      &= help "Directory to put generated exercises to serve. Default is 'exercise'"
        , logDir        = "log"   &= typDir         &= help "Directory to put log files in. Default is 'log'."
        , staticDir  = "static"      &= typDir         &= help "Files in this directory will be served as they are (for css and javascript files). Default is 'static'."

        , hoogledb      = Nothing      &= typFile        &= help "Hoogle database file"

        , mainpage      = "Index.xml" &= typ "PATH" &= help "Main web page path"
        , lang          = En    &= typ "LANGUAGE" &= help "Default language. It is 'en' by default."
        , port          = 8000    &= typ "PORT"     &= help "Port to listen"
        , recompilecmd  = "ghc -O" &= typ "COMMAND" &= help "Command to run before page generation. Default is 'ghc -O'."
        , magicname    = "a9xYf"  &= typ "VARNAME"  &= help "Magic variable name."
        , daemon       = False                      &= help "Run as a service."
        }  &= summary ("activehs " ++ showVersion version ++ ", (C) 2010-2012 Péter Diviánszky, 2017 Artúr Poór")
           &= program "activehs"

createDirs :: Args -> IO ()
createDirs (Args {sourceDir, genDir, exerciseDir, logDir}) 
    = mapM_ mkDir [sourceDir, genDir, exerciseDir, logDir]
 where
    mkDir path = when (not . null $ path) $
                   createDirectoryIfMissing True path


getArgs :: IO Args
getArgs = do
    args <- cmdArgs defaultArgs
    createDirs args
    return args



