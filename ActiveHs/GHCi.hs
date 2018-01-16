{-# LANGUAGE PatternGuards #-}

module ActiveHs.GHCi (
    GHCi
  , runGHCi
  , getI18N
  , getHoogleDb
  , EvaluationError(..)
  ) where

import           ActiveHs.Base (WrapData(WrapData), WrapData2(WrapData2))
import qualified ActiveHs.Hoogle as H
import qualified ActiveHs.Parser as P
import qualified ActiveHs.Result as R
import           ActiveHs.Specialize (specialize)
import           ActiveHs.Translation.I18N (I18N, translateParam1, translateParam1Str)
import qualified ActiveHs.Translation.Entries as E

-- ActiveHs-misc imports
import qualified Data.Data.Eval as Eval
import qualified Data.Data.Compare as C
import qualified Data.Data.GenRep as Gen
import qualified Data.Data.GenRep.Doc as GenDoc
import qualified Data.Data.GenRep.Functions as GenFun
import qualified Graphics.Diagrams as Dia
import qualified Graphics.Diagrams.FunctionGraphs as FunGraph
import qualified Graphics.Diagrams.SVG as SVG

import           Control.DeepSeq (force)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import qualified Control.Monad.Catch as MC
import           Control.Monad.Trans (lift, liftIO)
import qualified Data.Data as Data
import qualified Data.Dynamic as Dyn
import qualified Data.Text as T
import qualified Language.Haskell.Interpreter as GHC
import           System.FilePath (FilePath)
import qualified Text.XHtml as XH

type GHCi a = ReaderT GHCiContext GHC.Interpreter a

data GHCiContext = GHCiContext
  { g_i18n     :: I18N
  , g_hoogleDb :: Maybe FilePath
  }

evaluate :: P.Expression -> GHCi R.Result
--evaluate expr@(getCommand -> (cmd, arg)) = do
evaluate expression = do
  i18n <- getI18N
  force <$> P.expressionCata
              hoogle
              hoogleInfo
              (\expr -> do
                xx <- lift $ GHC.typeOf expr
                return $ R.ExprType False expr xx [])
              (\type_ -> do
                xx <- lift $ GHC.kindOf type_
                return $ R.TypeKind type_ xx [])
              (\expr -> (exprType expr >>= exprPpr) `catchE`
                \tyErr -> (typeKind expr) `orElse` (hoogleInfo expr) `orElse` MC.throwM tyErr)
              expression
{-        _   ->  return $ force $ Error True $ 
                   translateParam1 i18n (E.msg_Eval_NotSupported "The %s command is not supported.") (':':cmd)
-}

  where
    exprType :: String -> GHCi String
    exprType = lift . GHC.typeOf

    exprPpr :: String -> GHCi R.Result
    exprPpr expr = do
      ty <- exprType expr
      case specialize ty of
        Left err         -> return (R.Error True (T.pack err))
        Right (ty',ty'') -> do
          result <- (pprData expr ty'') `orElse` (ppr expr ty')
          case result of
            Nothing -> do
              i18n <- getI18N
              return $ R.Error
                        True
                        (translateParam1Str
                          i18n
                          (E.msg_Eval_DontKnowHowToEvaluate "I don't know how to evaluate this expression, but I can show its type: %s")
                          ty') -- or ty'' ??
            Just res ->
              return res

    pprData :: String -> String -> GHCi (Maybe R.Result)
    pprData expr type_ =  do
      wd <- lift $ GHC.interpret ("wrapData (" ++ GHC.parens expr ++ " :: " ++ type_ ++")") (GHC.as :: WrapData)
      liftIO (pprintData type_ wd)

    ppr :: String -> String -> GHCi (Maybe R.Result)
    ppr expr type_ = do
      dyn <- lift $ GHC.interpret ("toDyn (" ++ GHC.parens expr ++ " :: " ++ type_ ++")") (GHC.as :: Dyn.Dynamic)
      liftIO (pprint "" dyn)

    typeKind :: String -> GHCi R.Result
    typeKind expr = do
       k <- lift $ GHC.kindOf expr
       return $ R.TypeKind expr k []

    hoogle :: String -> GHCi R.Result
    hoogle term = do
      i18n <- getI18N
      getHoogleDb >>= maybe (noHoogle i18n term) (\db -> liftIO $ H.query db term)

    hoogleInfo :: String -> GHCi R.Result
    hoogleInfo term = do
      i18n <- getI18N
      getHoogleDb >>= maybe (noHoogle i18n term) (\db -> liftIO $ H.queryInfo i18n db term)

    noHoogle :: I18N -> String -> GHCi R.Result
    noHoogle i18n term = return (noInfo i18n term)

    orElse :: GHCi a -> GHCi a -> GHCi a
    orElse x y = x `catchE` \_ -> y

    catchE :: GHCi a -> (GHC.InterpreterError -> GHCi a) -> GHCi a
    catchE = MC.catch

    noInfo :: I18N -> String -> R.Result
    noInfo i18n query = R.Message (translateParam1Str i18n (E.msg_Eval_NoHoogleInfo "No info for %s") query) Nothing

loadFile :: String -> GHCi ()
loadFile filename = lift $ GHC.loadModules [filename]

runGHCi :: GHCi a -> I18N -> Maybe FilePath -> IO (Either EvaluationError a)
runGHCi m i18n hoogleDb = do
  result <- GHC.runInterpreter (runReaderT m (GHCiContext i18n hoogleDb))
  return $ case result of
             Right a -> Right a
             Left err -> Left (toEvaluationError err)
    where
      toEvaluationError :: GHC.InterpreterError -> EvaluationError
      toEvaluationError err = EvaluationError
        { generalInfo = i18n $ interpreterErrorCata 
                          (const (E.msg_Eval_WontCompile "Won't compile"))
                          (const (E.msg_Eval_UnknownError "Unknown error"))
                          (const (E.msg_Eval_NotAllowed "Not allowed"))
                          (const (E.msg_Eval_GhcException "GHCi exception"))
                          err
        , details = T.pack $
                      interpreterErrorCata
                        (unlines . map GHC.errMsg)  -- GHC.WontCompile
                        id                          -- GHC.UnknownError
                        id                          -- GHC.NotAllowed
                        id                          -- GHC.GhcException
                        err
        }

interpreterErrorCata :: ([GHC.GhcError] -> a) -> (String -> a) -> (String -> a) -> (String -> a) -> GHC.InterpreterError -> a
interpreterErrorCata wontCompile unknownError notAllowed ghcException err =
  case err of
    GHC.WontCompile l -> wontCompile l
    GHC.UnknownError s -> unknownError s
    GHC.NotAllowed s -> notAllowed s
    GHC.GhcException l -> ghcException l
       
data EvaluationError = EvaluationError
  { generalInfo :: T.Text
  , details     :: T.Text
  }

getI18N :: GHCi I18N
getI18N = asks g_i18n

getHoogleDb :: GHCi (Maybe FilePath)
getHoogleDb = asks g_hoogleDb

-----

pprintData :: String -> WrapData -> IO (Maybe R.Result)
pprintData y (WrapData x)
  | Data.dataTypeName (Data.dataTypeOf x) == "Diagram" =
      return Nothing
  | otherwise = do
      a <- Eval.eval 1 700 x
      let ([p], es) = GenFun.numberErrors [a]
      return . Just $ R.ExprType False (show $ GenDoc.toDoc p) y es

pprint :: String -> Dyn.Dynamic -> IO (Maybe R.Result)
pprint ident d
    | Just x <- Dyn.fromDynamic d = ff x
    | Just x <- Dyn.fromDynamic d = ff $ showFunc (x :: Double -> Double)
    | Just x <- Dyn.fromDynamic d = ff $ showFunc (x :: Double -> Integer)
    | Just x <- Dyn.fromDynamic d = ff $ showFunc $ fromIntegral . fromEnum . (x :: Double -> Bool)
    | Just x <- Dyn.fromDynamic d = ff $ showFunc $ fromIntegral . fromEnum . (x :: Double -> Ordering)
    | Just x <- Dyn.fromDynamic d = ff $ showFunc_ (x :: Integer -> Double)
    | Just x <- Dyn.fromDynamic d = ff $ showFunc_ (x :: Integer -> Integer)
    | Just x <- Dyn.fromDynamic d = ff $ showFunc_ $ fromIntegral . fromEnum . (x :: Integer -> Bool)
    | Just x <- Dyn.fromDynamic d = ff $ showFunc_ $ fromIntegral . fromEnum . (x :: Integer -> Ordering)
    | Just x <- Dyn.fromDynamic d = ff $ displayArc' (x :: Double -> (Double, Double))
    | Just (f,g) <- Dyn.fromDynamic d = ff $ displayArc' ((\x -> (f x, g x)) :: Double -> (Double, Double))
    | otherwise = return Nothing
 where
    ff :: Dia.Diagram -> IO (Maybe R.Result)
    ff = fmap g . SVG.render 10 (-16, -10) (16, 10) 5 2048 ident

    g :: (XH.Html, [(String, String)]) -> Maybe R.Result
    g (htm, err) = Just (R.Dia (XH.renderHtmlFragment htm) err)

    showFunc :: (RealFrac a, Real b) => (a -> b) -> Dia.Diagram
    showFunc = FunGraph.displayFun (-16,-10) (16,10)

    showFunc_ :: (Real b, Integral a) => (a -> b) -> Dia.Diagram
    showFunc_ = FunGraph.displayDiscreteFun (-16,-10) (16,10)
    
    displayArc' :: (Fractional a, Real b, Real c) => (a -> (b, c)) -> Dia.Diagram
    displayArc' = FunGraph.displayArc (-16,-10) (16,10) (0,1) 

------------------------

wrap2 :: String -> String -> String
wrap2 a b = "WrapData2 " ++ GHC.parens a ++ " " ++ GHC.parens b

----------------

compareMistGen :: I18N -> String -> WrapData2 -> String -> IO R.Result
compareMistGen i18n ident (WrapData2 x y) goodsol
    | Data.dataTypeName (Data.dataTypeOf x) == "Diagram" 
    = return $ R.Message (i18n $ E.msg_Eval_CantCompareDiagrams "Can't decide the equality of diagrams (yet).") Nothing
compareMistGen i18n ident (WrapData2 x y) goodsol = do
    (ans, a', b') <- C.compareData 0.8 0.2 700 x y
    return $ case ans of
        C.Yes -> R.Message (i18n $ E.msg_Eval_GoodSolution "Good solution! Another good solution:")
                          $ Just $ R.ExprType False goodsol "" []
        _ ->
            let x = case ans of
                    C.Maybe _  -> i18n (E.msg_Eval_CantDecide "I cannot decide whether this is a good solution:")
                    C.No       -> i18n (E.msg_Eval_WrongSolution "Wrong solution:")
            in R.Message x $ Just $ showPair ans (a', GenFun.mistify b')


---------------------------------

compareClearGen :: I18N -> String -> WrapData2 -> IO R.Result
compareClearGen i18n _ident (WrapData2 x y)
    | Data.dataTypeName (Data.dataTypeOf x) == "Diagram"
    = return $ R.Message (i18n $ E.msg_Eval_CantCompareDiagrams "Can't decide the equality of diagrams (yet).") Nothing
compareClearGen lang _ident (WrapData2 x y) = do
    (ans, a', b') <- C.compareData 0.8 0.2 700 x y
    return $ case ans of
--        C.Yes -> []
        _ -> showPair ans (a', b')


showPair :: C.Answer -> (Gen.GenericData, Gen.GenericData) -> R.Result
showPair x (a, b) = R.Comparison (show (GenDoc.toDoc a')) x (show (GenDoc.toDoc b')) es
  where ([a', b'], es) = GenFun.numberErrors [a, b]
