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
import           Control.Concurrent (forkIO)
import           Control.Exception (catch, IOException)
import           Control.Lens (makeLenses, view)
import           Control.Monad (void, forM_, forM)
import           Control.Monad.Trans (liftIO)
import           Data.IORef (IORef, readIORef, newIORef)
import qualified Lucid as L
import           Snap.Snaplet (Snaplet, Handler, SnapletInit)
import qualified Snap as S
import qualified Snap.Util.FileServe as SF
import           System.FilePath ((</>), (<.>), takeExtension, replaceExtension)
import           System.Directory (listDirectory, getModificationTime)

type ActiveHsHandler s a = Handler ActiveHsContext s a

data ActiveHsContext = ActiveHsContext
  { _ghciContext      :: Snaplet GHCiService
  , _logContext       :: Snaplet Logger
  }

makeLenses ''ActiveHsContext

initGhciService :: SnapletInit b GHCiService
initGhciService = S.makeSnaplet "ghci-service" "GHCi service" Nothing $ do
  let eval expr msg = GHCi.runGHCi (GHCi.eval expr) msg Nothing
  return $ GHCiService eval (const $ return ())

initLogService :: FilePath -> SnapletInit b Logger
initLogService p = S.makeSnaplet "log-service" "Log Service" Nothing $
  liftIO $ Logger.newLogger p

getGhciService :: ActiveHsHandler ActiveHsContext GHCiService
getGhciService = S.with ghciContext $ view S.snapletValue <$> S.getSnapletState

getLogger :: ActiveHsHandler ActiveHsContext Logger
getLogger = S.with logContext $ view S.snapletValue <$> S.getSnapletState

initActiveHsContext :: SnapletInit ActiveHsContext ActiveHsContext
initActiveHsContext = S.makeSnaplet "activehs" "ActiveHs" Nothing $ do
  ghci <- S.nestSnaplet "" ghciContext initGhciService
  logger <- S.nestSnaplet "" logContext (initLogService $ "log" </> "internal")

  dataDir <- liftIO $ Paths.getDataDir

  S.addRoutes [ ("static", S.method S.GET (SF.serveDirectoryWith SF.simpleDirectoryConfig (dataDir </> "static")))
              , ("", S.method S.GET servePage)
              ]
  
  S.addPostInitHook (\context -> do
                        forkIO $ convertAllFiles (view (ghciContext . S.snapletValue) context) (view (logContext . S.snapletValue) context)
                        return $ Right context
                    )
  
  return $ ActiveHsContext ghci logger

  where
    servePage :: ActiveHsHandler ActiveHsContext ()
    servePage = do
      p <- SF.getSafePath
      if null p
        then SF.serveFile "Index.html"
        else SF.serveFile (replaceExtension p ".html")

    writeHtml :: Bootstrap.Html -> ActiveHsHandler v ()
    writeHtml = S.writeLazyText . L.renderText

    notFound :: ActiveHsHandler v ()
    notFound = do
      writeHtml $ Bootstrap.bootstrapPage "Error" (Bootstrap.col4Offset4 "Page not found")
      S.getResponse >>= S.finishWith . S.setResponseCode 404

    convertAllFiles :: GHCiService -> Logger -> IO ()
    convertAllFiles ghci logger = do
      lhsFiles <- filter ((== ".lhs") . takeExtension) <$> listDirectory "."
      forM_ lhsFiles $ \f -> do
        let outDir = "."
            objectFile = replaceExtension f ".o"
            html   = replaceExtension f ".html"
        fModTime <- getModificationTime f
        outdated <- forM [objectFile, html] $ \p ->
                      catch
                        ((< fModTime) <$> getModificationTime p)
                        (const (return True) :: IOException -> IO Bool)
        if or outdated
          then void $ C.convert f outDir logger ghci
          else return ()

type GETHandler  = ActiveHsHandler ActiveHsContext ()
