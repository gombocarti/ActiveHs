module ActiveHs.Logger (
      SF.Logger
    , SF.newLogger
    , LogLevel (..)
    , logMessage
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.ByteString.UTF8 as BsUTF8
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified System.FastLogger as SF

------

data LogLevel =
    DEBUG
  | INFO
  | ERROR
  deriving Show

logMessage :: MonadIO m => LogLevel -> SF.Logger -> T.Text -> m ()
logMessage level logger = liftIO . SF.logMsg logger . T.encodeUtf8 . T.append (T.pack $ show level ++ " ")
