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

import ActiveHs.Base (WrapData2(WrapData2))

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.XHtml.Strict ((+++))

import Control.DeepSeq
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch)
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
    | Compare2 T.Text [String] String -- Environment (imports, type declarations), defined function names, expression to evaluate
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
    eval Eval = renderResult <$> evaluate ch lang fn (T.unpack sol)

    eval (Compare hiddenname goodsol) = do
      typed <- runInterpreter ch lang fn $
        interpret (wrapData2 (T.unpack sol) hiddenname) (as :: WrapData2)
      case typed of
        Success (WrapData2 typedSol typedCorrectAnswer) ->
          renderResult <$> compareMistGen lang (show m5) typedSol typedCorrectAnswer goodsol
        Failure err ->
          return . renderResult $ Error False err

    eval (Compare2 env funnames expr) = do
        fn' <- tmpSaveHs "hs" (show m5) $ env `T.append` sol
        renderResult <$>
          case qualify qualifier funnames expr of
              Left _err ->
                evaluate ch lang fn' expr
              Right exprForUsersSolution -> do
                res <- runInterpreter ch lang fn' $
                  interpret (wrapData2 expr exprForUsersSolution) (as :: WrapData2)
                case res of
                  Success (WrapData2 correctAnswer usersAnswer) ->
                    compareClearGen lang correctAnswer usersAnswer
                  Failure err ->
                    return $ Error False err

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
    T.writeFile tmp s
    return tmp
