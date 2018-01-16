module ActiveHs.Logger (
      SF.Logger
    , SF.newLogger
    , LogLevel (..)
    , logMessage
    ) where

import qualified Data.ByteString.UTF8 as BsUTF8
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified System.FastLogger as SF

------

data LogLevel =
    DEBUG
  | INFO
  | ERROR
  deriving Show

logMessage :: MonadIO m => LogLevel -> SF.Logger -> String -> m ()
logMessage level logger = liftIO . SF.logMsg logger . BsUTF8.fromString . ((show level ++ " ") ++)
