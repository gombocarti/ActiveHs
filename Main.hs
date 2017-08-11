{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, PatternGuards, NamedFieldPuns #-}

module Main where

import Smart hiding (hoogledb)
import Cache
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
mainWithArgs args@(Args {verbose, port, static, logdir, hoogledb, fileservedir, gendir, mainpage, restartpath, sourcedir, includedir, daemon}) = do 

    log <- newLogger verbose $ 
        logdir </> "interpreter.log"

    ch <- startGHCiServer [sourcedir] log hoogledb
    cache <- newCache 10

    logStrMsg 2 log ("lang: " ++ lang args)

    let mainLogic = httpServe
                      ( setPort port
                      . setAccessLog (if null logdir then ConfigNoLog else ConfigFileLog (logdir </> "access.log"))
                      . setErrorLog  (if null logdir then ConfigNoLog else ConfigFileLog (logdir </> "error.log"))
                      $ emptyConfig
                      )
                      (  method GET
                         (   serveDirectoryWith simpleDirectoryConfig fileservedir
                         <|> serveHtml ch
                         <|> ifTop (redirectString mainpage)
                         <|> pathString restartpath (liftIO $ restart ch >> clearCache cache)
                         )
                         <|> method POST (exerciseServer (sourcedir:includedir) (cache, ch) args)
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

type TaskChan' = (Cache (Int, T.Text), TaskChan)

exerciseServer :: [FilePath] -> TaskChan' -> Args -> Snap ()
exerciseServer sourcedirs (cache, ch) args@(Args {magicname, lang, exercisedir, verboseinterpreter}) = do
    params <- fmap show getParams
    when (length params > 3000) $ do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Too long request.</body></html>"
        getResponse >>= finishWith . setResponseCode 400

    let md5Id = mkHash params   -- could be more efficient
    liftIO $ logStrMsg 2 (logger ch) $ " eval " ++ show md5Id ++ " " ++ params
    j <- liftIO $ lookupCache cache md5Id
    (>>= writeText) $ case j of
      Left (delay, res) -> liftIO $ do
        logStrMsg 2 (logger ch) $ "   ch " ++ show md5Id
        threadDelay delay
        return res
      Right cacheAction -> do
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
                liftIO $ exerciseServer' ('X':magicname) ch verboseinterpreter fn x lang' md5Id task
              else
                return (inconsistencyError lang')
         <|> return (inconsistencyError lang)

        liftIO $ do
            time' <- getCurrentTime
            let delay = round $ 1000000 * (realToFrac $ diffUTCTime time' time :: Double) :: Int
            logStrMsg 2 (logger ch) $ "  end " ++ show md5Id ++ " " ++ show delay ++ " ms  " ++ T.unpack res
            cacheAction (delay, res)
            return res

  where
    inconsistencyError :: String -> Html
    inconsistencyError lang' = renderResult $ Error True $ translate lang' "Inconsistency between server and client."

    eval_ :: String -> T.Text -> T.Text -> [T.Text] -> Maybe SpecialTask
    eval_ _ "eval"  _ [_]
        = Just Eval
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

maybeRead :: Read a => String -> Maybe a
maybeRead s = listToMaybe [a | (a,"") <- reads s] 

re :: Read b => T.Text -> Maybe b
re = maybeRead . T.unpack


