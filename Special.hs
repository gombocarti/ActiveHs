{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, PatternGuards, NamedFieldPuns, CPP #-}

module Special
    ( SpecialTask (..), exerciseServer'
    ) where

import Smart
import QuickCheck
import Result
import Lang
import Logger
import Html
import Qualify (qualify)
import Hash

import ActiveHs.Base (WrapData2)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.XHtml.Strict ((+++))

import Control.DeepSeq
import Control.Concurrent.MVar
import Control.Exception
import System.FilePath ((</>),takeFileName)
import System.Directory (getTemporaryDirectory)

import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Monad.Trans (liftIO)
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

---------------------------------------------------------------

timeout :: forall b. Int -> IO b -> IO b -> IO b
timeout delay error action = do
    v <- newEmptyMVar 
    t1 <- forkIO $ threadDelay delay >> error >>= putMVar v
    t2 <- forkIO $ action >>= putMVar v
    x <- takeMVar v
    killThread t1
    killThread t2
    return x

----------------------------

data SpecialTask
    = Eval
    | Compare String String
    | Compare2 T.Text [String] String
    | Check String [FilePath] T.Text [String] [([String],String)] String String

exerciseServer' 
    :: String
    -> TaskChan
    -> Bool
    -> FilePath
    -> T.Text
    -> Language
    -> Hash
    -> SpecialTask
    -> IO Html

exerciseServer' qualifier ch verbose fn sol lang m5 task = do
    let error = do
            logStrMsg 0 (logger ch) $ unwords ["Timed out:", T.unpack sol, show m5]
            return $ renderResult $ Error True "Timed out."

        action = eval task `catch` \(e :: SomeException) ->             -- ???
                  return $ renderResult $ Error True $ show e

    timeout (10*1000000) error action

  where

    eval Eval
        = renderResult <$> interp verbose m5 lang ch fn (T.unpack sol) Nothing

    eval (Compare hiddenname goodsol)
        = do
            res <- interp verbose m5 lang ch fn (T.unpack sol) $ Just $ \a -> do
                     x <- interpret (wrap2 a hiddenname) (as :: WrapData2)
                     liftIO $ compareMistGen lang (show m5) x $ goodsol
            return $ renderResult res

    eval (Compare2 env funnames s) = do
        fn' <- tmpSaveHs "hs" (show m5) $ env `T.append` sol
        case qualify qualifier funnames s of
            Left err -> return $ renderResult (Error True err)
            Right s2 -> do
                res <- interp verbose m5 lang ch fn' s $ Just $ \a -> do
                         result <- interpret (wrap2 a s2) (as :: WrapData2)
                         liftIO $ compareClearGen lang (show m5) result
                return $ renderResult res

    eval (Check ext sourcedirs env funnames is i j) = do
        fn' <- tmpSaveHs ext (show m5) $ env `T.append` sol
        case ext of
            "hs" -> do
                ss <- quickCheck qualifier m5 lang ch fn' (T.unpack sol) funnames is
                case ss of
                  ShowFailedTestCase testcase reason ->
                    return . indent . renderResult $ ShowInterpreter lang 59 (getTwo "eval2" (takeFileName fn) j i j) j 'E' testcase (Just reason)
                  Message _ _ ->
                    return . indent $
                      renderResult ss
                      +++ (renderResult (ShowInterpreter lang 59 (getTwo "eval2" (takeFileName fn) j i j) j 'E' "" Nothing))
                  _ ->
                    return . indent $ renderResult ss

tmpSaveHs :: String -> String -> T.Text -> IO FilePath
tmpSaveHs ext x s = do
    tmpdir <- getTemporaryDirectory
    let name = "GHCiServer_" ++ x
        tmp = tmpdir </> name ++ "." ++ ext
    T.writeFile tmp $ case ext of
        "hs" -> s
    return tmp




