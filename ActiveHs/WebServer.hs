{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, PatternGuards, NamedFieldPuns #-}

module ActiveHs.WebServer (main) where

import ActiveHs.Bootstrap ()

import Smart hiding (hoogledb)
--import Cache
import ActiveHs.Converter
import ActiveHs.Args (Args)
import qualified ActiveHs.Args
--import Special
import qualified ActiveHs.Translation.Entries as E
import qualified ActiveHs.Translation.Base as B
import           ActiveHs.Translation.I18N (I18N)
import qualified ActiveHs.Translation.I18N as I18N
import ActiveHs.Logger
import ActiveHs.Result
--import Hash

import Snap
import Snap.Core
import Snap.Http.Server (httpServe)
import Snap.Http.Server.Config
import Snap.Util.FileServe (getSafePath, serveDirectoryWith, simpleDirectoryConfig)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory (doesFileExist)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<|>))
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)
import qualified Text.Blaze.Html5 as H

---------------------------------------------------------------

data ActiveHsConfig = ActiveHsConfig
  { language        :: B.Language
  , directoryConfig :: DirectoryConfig
  , port            :: Int
  , compileCmd      :: String
  , hoogleDb        :: Maybe FilePath
  , mainpage        :: FilePath
  }

data DirectoryConfig = DirectoryConfig
  { sourceDir   :: FilePath
  , genDir      :: FilePath
  , exerciseDir :: FilePath
  , logDir      :: FilePath
  , staticDir   :: FilePath
  }

newtype ProgramWithArgs = ProgramWithArgs (String, [String])

main :: IO ()
main = do
  args <- Args.getArgs

  checkArgs args
  log <- newLogger (Args.logdir args </> "interpreter.log")

  let i18n = I18N.mkI18N lang
  ch <- startGHCiServer [sourcedir] log hoogledb
  cache <- newCache 10

     let mainLogic = httpServe
                      ( setPort (Args.port args)
                      . setAccessLog $ ConfigFileLog (logdir </> "access.log")
                      . setErrorLog $ ConfigFileLog (logdir </> "error.log")
                      $ emptyConfig
                      )
                      (  method GET
                         ( serveDirectory fileservedir
                           <|> serveHtml i18n ch
                         )
                         <|> method POST (handleRequest evaluationConfig)
                         <|> notFound
                      )

    if daemon
      then mainLogic
      else do
        putStrLn "Press any key to stop the server."
        t <- forkIO mainLogic
        hSetBuffering stdin NoBuffering
        _ <- getChar
        killThread t
  where
    serveHtml :: MonadSnap m => I18N -> TaskChan -> m ()
    serveHtml i18n ch = do
        p <- getSafePath
        when (not static) $ liftIO $
            convert i18n ch args $ dropExtension p
        serveDirectoryWith simpleDirectoryConfig gendir

    notFound :: Snap ()
    notFound = do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Page not found.</body></html>"
        getResponse >>= finishWith . setResponseCode 404

check :: Bool -> String -> IO ()
check pred msg
  | pred      = return ()
  | otherwise = do
      putStrLn $ unwords ["ERROR", msg]
      exitFailure

checkArgs :: Args -> IO ()
checkArgs args = do
  let dirs = directoryConfig args
  check (port args > 0) "port is a negative number"
  checkIfDirExists (sourceDir dirs)
  checkIfDirExists (staticDir dirs)
  check (not . null $ recompileCmd args) "compilation command is an empty string"
  putStrLn "Arguments are OK!"

  where
    checkIfDirExists :: FilePath -> IO ()
    checkifDirExists path = do
      exists <- Dir.doesDirectoryExists path
      check exists $ unwords [path, "does not exist"]

type PostHandler = ActiveHs ()

evalHandler :: POSTHandler
evalHandler = do
    params <- fmap show getParams
    when (length params > 3000) $ do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Too long request.</body></html>"
        getResponse >>= finishWith . setResponseCode 400

    let i18n = I18N.mkI18N B.Hu
    logMsg INFO (logger ch) $ "user evaluates " ++ params
    time <- liftIO $ getCurrentTime
    res <- fmap renderHtml $ do
      Just [ss, fn_, x, y, T.unpack -> lang']  <- fmap sequence $ mapM getTextParam ["c","f","x","y","lang"]

      let fn = exercisedir </> T.unpack fn_
          ext = case takeExtension fn of
                  ('.':ext) -> ext
                  _         -> ""
      fnExists <- liftIO $ doesFileExist fn
      if fnExists
        then do
          Just task <- liftIO $ fmap (eval_ ext ss y . T.splitOn (T.pack delim)) $ T.readFile fn
          handleTask ('X':magicname) ch fn x task
        else
          return (inconsistencyError i18n)
    writeText res
  where
    inconsistencyError :: I18N -> H.Html
    inconsistencyError i18n = undefined -- renderResult $ Error True $ i18n $ E.msg_WebServer_Inconsistency "Inconsistency between server and client."

    eval_ :: String -> T.Text -> T.Text -> [T.Text] -> Maybe SpecialTask
    eval_ _ "eval"  y [_]
        = Just (Eval (T.unpack y)) --- ???
    eval_ _ "eval"  _ [_, goodsol]
        = Just $ Compare magicname $ T.unpack $ T.drop (length magicname + 4) $ goodsol
    eval_ ext comm
      (T.unpack -> s) 
      [env, hidden, re -> Just (is :: [([String],String)]), T.unpack -> j, T.unpack -> i, re -> Just funnames] 
        = Just $ case comm of 
            "eval2" -> Compare2 env funnames s
            "check" -> Check ext sourcedirs env funnames is i j
    eval_ _ _ _ _
        = Nothing

    magicname = undefined

re :: Read b => T.Text -> Maybe b
re = readMaybe . T.unpack

