{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ActiveHs.ActiveHsContext where

import ActiveHs.Converter (ConversionError)
import qualified ActiveHs.Converter as C
import ActiveHs.GHCi (EvaluationError, GHCiService(GHCiService))
import qualified ActiveHs.GHCi as GHCi
import ActiveHs.Logger (Logger)
import ActiveHs.Parser (Expression)
import ActiveHs.Result (Result)
import ActiveHs.Translation.I18N (I18N)

import Control.Lens (makeLenses, view)
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, readIORef, newIORef)
import Snap.Snaplet (Snaplet, Handler, SnapletInit)
import qualified Snap.Snaplet as S

type ActiveHsHandler s a = Handler ActiveHsContext s a

newtype SnapContext a = SnapContext (IORef a)

makeSnapContext :: a -> IO (SnapContext a)
makeSnapContext = fmap SnapContext . newIORef

type GhciContext      = SnapContext GHCiService
type ConverterContext = SnapContext ConverterService

newtype ConverterService = ConverterService
  { convert :: FilePath -> FilePath -> Logger -> GHCiService -> FilePath -> IO (Either ConversionError ())
  }

data ActiveHsContext = ActiveHsContext
  { _ghciContext      :: Snaplet GhciContext
  , _converterContext :: Snaplet ConverterContext
  }

makeLenses ''ActiveHsContext

initGhciService :: SnapletInit b GhciContext
initGhciService = S.makeSnaplet "ghci-service" "GHCi service" Nothing $ do
  let eval expr msg = GHCi.runGHCi (GHCi.eval expr) msg Nothing
  liftIO . makeSnapContext $ GHCiService eval (const $ return ())

initConverterService :: FilePath -> FilePath -> Logger -> SnapletInit b ConverterContext
initConverterService sourceDir genDir logger =
  S.makeSnaplet "converter-service" "Converter service" Nothing $
    liftIO . makeSnapContext $ ConverterService C.convert

getGhciService :: ActiveHsHandler ActiveHsContext GHCiService
getGhciService = S.with ghciContext $ do
  s <- S.getSnapletState
  let SnapContext mvar = view S.snapletValue s
  liftIO $ readIORef mvar

initActiveHsContext :: Logger -> SnapletInit b ActiveHsContext
initActiveHsContext logger = S.makeSnaplet "activehs" "ActiveHs" Nothing $ do
  ghci <- S.nestSnaplet "" ghciContext initGhciService
  converter <- S.nestSnaplet "" converterContext (initConverterService "" "" logger)
  return $ ActiveHsContext ghci converter

