{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ActiveHs.ActiveHsContext where

import ActiveHs.Converter (ConversionError)
import ActiveHs.GHCi (EvaluationError)
import ActiveHs.Parser (Expression)
import ActiveHs.Result (Result)
import ActiveHs.Translation.I18N (I18N)

import Control.Lens (makeLenses)
import Data.IORef (IORef, readIORef)
import Snap.Snaplet (Snaplet, Handler)

type ActiveHsHandler s a = Handler ActiveHsContext s a

newtype SnapContext a = SnapContext (IORef a)

data GhciService = GhciService
  { evaluate     :: Expression -> I18N -> IO (Either EvaluationError Result)
--  , testSolution :: FilePath -> String -> I18N -> IO (Either EvaluationError Result)
--  , reload       :: FilePath -> IO ()
  }

type GhciContext = SnapContext GhciService

data ActiveHsContext = ActiceHsContext
  { _ghciContext      :: Snaplet GhciContext
  , _converterContext :: Snaplet ConverterContext
  }

makeLenses ''ActiveHsContext

type ConverterContext = SnapContext ConverterService

newtype ConverterService = ConverterService
  { convert :: String -> IO (Either ConversionError ())
  }

initGhciService :: Maybe FilePath -> SnapletInit b GhciContext
initGhciService hoogleDb = makeSnaplet "ghci-service" "GHCi service" Nothing $ do
 let eval expr msg = runGHCi (G.evaluate expr) msg hoogleDb
     testSolution file solution msg = runGHCi

getGhciService :: ActiveHsHandler GhciContext GhciService
getGhciService = with ghciContext $ do
  s <- getSnapletState
  let SnapContext mvar = snapletValue s
  liftIO $ readIORef mvar
