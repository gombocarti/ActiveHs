{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, PatternGuards, FlexibleContexts, CPP #-}

module Simple
    ( Task (..), TaskChan
    , startGHCiServer
    , restartGHCiServer
    , sendToServer
    , catchError_fixed

    , Interpreter, typeOf, kindOf
    , InterpreterError (..), errMsg, interpret
    , MonadInterpreter
    , as, parens
    ) where

import Logger

import Language.Haskell.Interpreter

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException)
import qualified Control.Exception as CE
import Control.Monad (when, forever)
import Control.Monad.Catch (catch)
import Data.List (isPrefixOf)
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

-------------------------

data Task 
    = forall a. Task FilePath (MVar (Either InterpreterError a)) (Interpreter a)

newtype TaskChan 
    = TC (Chan (Maybe Task))

---------------

startGHCiServer :: [String] -> Logger -> IO TaskChan
startGHCiServer paths{-searchpaths-} log = do
    ch <- newChan 

    _ <- forkIO $ forever $ do
        logStrMsg 1 log "start interpreter"
        e <- runInterpreter (handleTask ch Nothing)
              `CE.catch` \(e :: SomeException) ->
                return $ Left $ UnknownError $ "GHCi server died: " ++ show e
        case e of
            Left  e  -> logStrMsg 0 log $ "stop interpreter: " ++ show e
            Right () -> return ()

    return $ TC ch

  where
    handleTask :: Chan (Maybe Task) -> Maybe FilePath -> Interpreter ()
    handleTask ch oldFn = do
        task <- lift $ readChan ch
        case task of
            Just task -> handleTask_ ch oldFn task
            Nothing   -> liftIO $ logStrMsg 0 log "interpreter stopped intentionally"

    handleTask_ ch oldFn (Task fn repVar m) = do
        (cont, res) <- do  
            when (oldFn /= Just fn) $ do
                reset
                set [searchPath := paths]
                set [languageExtensions := [ExtendedDefaultRules]]
                loadModules [fn]
                setTopLevelModules ["Main"]
            x <- m
            return (True, Right x)

          `catchError_fixed` \er -> do
            return (not $ fatal er, Left er)

        lift $ putMVar repVar res
        when cont $ handleTask ch $ case res of
            Right _ -> Just fn
            Left  _ -> Nothing


restartGHCiServer :: TaskChan -> IO ()
restartGHCiServer (TC ch) = writeChan ch Nothing

sendToServer :: TaskChan -> FilePath -> Interpreter a -> IO (Either InterpreterError a)
sendToServer (TC ch) fn m = do
    rep <- newEmptyMVar
    writeChan ch $ Just $ Task fn rep m
    takeMVar rep

fatal :: InterpreterError -> Bool
fatal (WontCompile _) = False
fatal (NotAllowed _)  = False
fatal _ = True

catchError_fixed
    :: MonadInterpreter m
    => m a -> (InterpreterError -> m a) -> m a
m `catchError_fixed` f = m `catch` (f . fixError)
  where
    fixError (UnknownError s) 
        | Just x <- dropPrefix "GHC returned a result but said: [GhcError {errMsg =" s
        = WontCompile [GhcError {errMsg = case reads x of ((y,_):_) -> y; _ -> s}]
    fixError x = x

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix s m
    | s `isPrefixOf` m = Just $ drop (length s) m
    | otherwise = Nothing



