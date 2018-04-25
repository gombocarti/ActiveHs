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
import qualified Data.HashSet as Set
import Control.Monad (when, forever)
import Control.Monad.Catch (catch)
import Data.Monoid ((<>), Endo(Endo, appEndo))
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

startGHCiServer :: [String] -> Logger -> Int -> IO TaskChan
startGHCiServer paths log resetsPerRuns = do
    ch <- newChan 

    _ <- forkIO $ forever $ do
        logStrMsg 1 log "start interpreter"
        result <- runInterpreter (getTask ch Nothing resetsPerRuns)
                   `CE.catch` \(e :: SomeException) ->
                     return $ Left $ UnknownError $ "GHCi server died: " ++ show e
        case result of
            Left  e  -> logStrMsg 0 log $ "stop interpreter: " ++ show e
            Right () -> return ()

    return $ TC ch

  where
    getTask :: Chan (Maybe Task) -> Maybe FilePath -> Int -> Interpreter ()
    getTask ch oldFn resetsLeft = do
        task <- lift $ readChan ch
        case task of
            Just task -> handleTask ch oldFn task resetsLeft
            Nothing   -> liftIO $ logStrMsg 0 log "interpreter stopped intentionally"

    handleTask ch oldFn (Task fn repVar m) resetsLeft = do
        let resetNeeded = oldFn /= Just fn
            resetsLeft' = if resetNeeded then resetsLeft - 1 else resetsLeft
        (cont, res) <- do
            when resetNeeded $ do
                reset
                set [searchPath := paths]
                set [languageExtensions := [ExtendedDefaultRules]]
                loadModules [fn]
                setTopLevelModules ["Main"]
            x <- m
            return (True, Right x)

          `catchError_fixed` \er -> do
            return (not $ fatal er, Left $ eachErrorOnce er)

        lift $ putMVar repVar res
        let newFn = case res of
              Right _ -> Just fn
              Left  _ -> Nothing
        when (cont && resetsLeft' > 0) $ getTask ch newFn resetsLeft'

       where
         -- Removes duplicated error messages in WontCompile.
         -- Duplicates arise when there is a compilation error in the file to be loaded.
         eachErrorOnce :: InterpreterError -> InterpreterError
         eachErrorOnce (WontCompile errs) =
           WontCompile . map GhcError . removeDuplicates $ map errMsg errs
         eachErrorOnce err = err

         removeDuplicates :: [String] -> [String]
         removeDuplicates ss = appEndo (fst $ foldl insertIfNotOccured (mempty, Set.empty) ss) []
           where
             insertIfNotOccured :: (Endo [String], Set.HashSet String) -> String -> (Endo [String], Set.HashSet String)
             insertIfNotOccured acc@(ss', occured) s
               | Set.member s occured = acc
               | otherwise            = (ss' <> Endo (s :), Set.insert s occured)


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



