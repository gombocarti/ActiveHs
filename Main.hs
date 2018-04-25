{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, PatternGuards, NamedFieldPuns #-}

module Main where

import Smart hiding (hoogledb)
import Converter
import Args
import Special
import Lang
import Logger
import Result
import Html
import Hash
import Snap

import Snap.Core
import Snap.Http.Server (httpServe)
import Snap.Http.Server.Config
import Snap.Util.FileServe (getSafePath, serveDirectoryWith, simpleDirectoryConfig)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory (doesFileExist)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<|>))
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Maybe (listToMaybe)

---------------------------------------------------------------

main :: IO ()
main = getArgs >>= mainWithArgs

mainWithArgs :: Args -> IO ()
mainWithArgs args@(Args {verbose, port, static, logdir, hoogledb, staticdir, publicdir, gendir, mainpage, restartpath, sourcedir, includedir, daemon, reloadsPerGhciSession}) = do 

    log <- newLogger verbose $ 
        logdir </> "interpreter.log"

    ch <- startGHCiServer [sourcedir] log hoogledb reloadsPerGhciSession

    logStrMsg 2 log ("lang: " ++ lang args)

    let mainLogic = httpServe
                      ( setPort port
                      . setAccessLog (if null logdir then ConfigNoLog else ConfigFileLog (logdir </> "access.log"))
                      . setErrorLog  (if null logdir then ConfigNoLog else ConfigFileLog (logdir </> "error.log"))
                      $ emptyConfig
                      )
                      (  method GET
                         (   serveDirectoryWith simpleDirectoryConfig staticdir
                         <|> serveDirectoryWith simpleDirectoryConfig publicdir
                         <|> serveHtml ch
                         <|> ifTop (redirectString mainpage)
                         <|> pathString restartpath (liftIO $ restart ch)
                         )
                         <|> method POST (exerciseServer (sourcedir:includedir) ch args)
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
    serveHtml ch = do
        p <- getSafePath
        when (not static && takeExtension p `elem` [".xml"]) $ liftIO $
            convert ch args $ dropExtension p
        serveDirectoryWith simpleDirectoryConfig gendir

    notFound :: Snap ()
    notFound = do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Page not found.</body></html>"
        getResponse >>= finishWith . setResponseCode 404


---------------------------------------------------------------

exerciseServer :: [FilePath] -> TaskChan -> Args -> Snap ()
exerciseServer sourcedirs ch args@(Args {magicname, lang, exercisedir, verboseinterpreter}) = do
    params <- fmap show getParams
    when (length params > 3000) $ do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Too long request.</body></html>"
        getResponse >>= finishWith . setResponseCode 400

    let md5Id = mkHash params   -- could be more efficient
    liftIO $ logStrMsg 2 (logger ch) $ " eval " ++ show md5Id ++ " " ++ params
--  (>>= writeText)
    res <- (do
               Just [cmd, fn_, solution, trial, T.unpack -> lang'] <- fmap sequence $ mapM getTextParam ["c","f","x","y","lang"]

               let fn = exercisedir </> T.unpack fn_
                   ext = case takeExtension fn of
                           ('.':ext) -> ext
                           _         -> ""
               fnExists <- liftIO $ doesFileExist fn
               if fnExists
                 then do
                   Just task <- liftIO $ fmap (eval_ ext cmd trial . T.splitOn (T.pack delim)) $ T.readFile fn
                   liftIO $ exerciseServer' ('X':magicname) ch verboseinterpreter fn solution lang' md5Id task
                 else
                   return (inconsistencyError lang'))
           <|> return (inconsistencyError lang)
    writeText (renderHtml res)

  where
    inconsistencyError :: String -> Html
    inconsistencyError lang' = renderResult $ Error True $ translate lang' "Inconsistency between server and client."

    eval_ :: String -> T.Text -> T.Text -> [T.Text] -> Maybe SpecialTask
    eval_ _ "eval"  _ [_]
        = Just Eval
    eval_ _ "eval"  _ [_, goodsol]
        = Just $ Compare magicname $ T.unpack $ T.drop (length magicname + 4) $ goodsol
    eval_ ext comm
      (T.unpack -> expr)
      -- hidden is the solution usually
      [env, _hidden, re -> Just (tests :: [([String],String)]), T.unpack -> j, T.unpack -> i, re -> Just funnames] 
        = case comm of
            "eval2" -> Just $ Compare2 env funnames expr
            "check" -> Just $ Check ext sourcedirs env funnames tests i j
            _ -> Nothing
    eval_ _ _ _ _
        = Nothing

maybeRead :: Read a => String -> Maybe a
maybeRead s = listToMaybe [a | (a,"") <- reads s] 

re :: Read b => T.Text -> Maybe b
re = maybeRead . T.unpack


