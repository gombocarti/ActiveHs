{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ActiveHs.ActiveHsContext where

import qualified ActiveHs.Bootstrap as Bootstrap
import           ActiveHs.Converter (ConversionError)
import qualified ActiveHs.Converter as C
import           ActiveHs.GHCi (EvaluationError, GHCiService(GHCiService))
import qualified ActiveHs.GHCi as GHCi
import           ActiveHs.Logger (Logger)
import qualified ActiveHs.Logger as Logger
import           ActiveHs.Parser (Expression)
import           ActiveHs.Result (Result)
import           ActiveHs.Translation.I18N (I18N)

import qualified Paths_activehs as Paths

import           Control.Applicative ((<|>))
import           Control.Lens (makeLenses, view)
import           Control.Monad.Trans (liftIO)
import           Data.IORef (IORef, readIORef, newIORef)
import qualified Lucid as L
import           Snap.Snaplet (Snaplet, Handler, SnapletInit)
import qualified Snap as S
import qualified Snap.Util.FileServe as SF
import           System.FilePath ((</>), (<.>), takeExtension)

type ActiveHsHandler s a = Handler ActiveHsContext s a

newtype SnapContext a = SnapContext (IORef a)

makeSnapContext :: a -> IO (SnapContext a)
makeSnapContext = fmap SnapContext . newIORef

type GhciContext      = SnapContext GHCiService
type LogContext       = SnapContext LogService

newtype LogService = LogService { unLogService :: Logger }

data ActiveHsContext = ActiveHsContext
  { _ghciContext      :: Snaplet GhciContext
  , _logContext       :: Snaplet LogContext
  }

makeLenses ''ActiveHsContext

initGhciService :: SnapletInit b GhciContext
initGhciService = S.makeSnaplet "ghci-service" "GHCi service" Nothing $ do
  let eval expr msg = GHCi.runGHCi (GHCi.eval expr) msg Nothing
  liftIO . makeSnapContext $ GHCiService eval (const $ return ())

initLogService :: FilePath -> SnapletInit b LogContext
initLogService p = S.makeSnaplet "log-service" "Log Service" Nothing $
  liftIO $ do
    logger <- Logger.newLogger p
    makeSnapContext $ LogService logger

getGhciService :: ActiveHsHandler ActiveHsContext GHCiService
getGhciService = S.with ghciContext $ do
  s <- S.getSnapletState
  let SnapContext mvar = view S.snapletValue s
  liftIO $ readIORef mvar

getLogger :: ActiveHsHandler ActiveHsContext Logger
getLogger = S.with logContext $ do
  s <- S.getSnapletState
  let SnapContext mvar = view S.snapletValue s
  unLogService <$> liftIO (readIORef mvar)

initActiveHsContext :: SnapletInit ActiveHsContext ActiveHsContext
initActiveHsContext = S.makeSnaplet "activehs" "ActiveHs" Nothing $ do
  ghci <- S.nestSnaplet "" ghciContext initGhciService
  logger <- S.nestSnaplet "" logContext (initLogService $ "log" </> "internal")

  dataDir <- liftIO $ Paths.getDataDir

  S.addRoutes [ ("static", SF.serveDirectoryWith SF.simpleDirectoryConfig (dataDir </> "static"))
              , ("", S.method S.GET servePage <|> notFound)
              ]
  
  return $ ActiveHsContext ghci logger

  where
    servePage :: ActiveHsHandler ActiveHsContext ()
    servePage = do
      p <- SF.getSafePath
      ghci <- getGhciService
      logger <- getLogger
      res <- liftIO $ C.convert p "." logger ghci
      either showConversionError SF.serveFile res

      where
        showConversionError :: C.ConversionError -> GETHandler
        showConversionError convError = do -- TODO I18N
          writeHtml $ Bootstrap.bootstrapPage "Error" (Bootstrap.alert Bootstrap.Error "Error during conversion")
          S.finishWith =<< S.setResponseCode 500 <$> S.getResponse

    writeHtml :: Bootstrap.Html -> ActiveHsHandler v ()
    writeHtml = S.writeLazyText . L.renderText

    notFound :: ActiveHsHandler v ()
    notFound = do
      writeHtml $ Bootstrap.bootstrapPage "Error" (Bootstrap.col4Offset4 "Page not found")
      S.getResponse >>= S.finishWith . S.setResponseCode 404


type GETHandler  = ActiveHsHandler ActiveHsContext ()
