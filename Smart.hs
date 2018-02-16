{-# LANGUAGE ViewPatterns, PatternGuards #-}
module Smart
    ( module Simple
    , startGHCiServer
    , restart
    , TaskChan (..)
    , interp
    , compareClearGen
    , compareMistGen
    , wrap2
    ) where

import HoogleCustom
import Specialize
import Lang
import Result
import Logger
import Simple hiding (TaskChan, startGHCiServer)
import qualified Simple
import Hash

import ActiveHs.Base (WrapData2 (..), WrapData(..))
import Graphics.Diagrams (Diagram)
import Graphics.Diagrams.SVG (render)
import Graphics.Diagrams.FunctionGraphs (displayFun, displayDiscreteFun, displayArc)

import qualified Data.Data.Eval as C
import qualified Data.Data.Compare as C
import Data.Data.GenRep hiding (Error)
import Data.Data.GenRep.Functions (mistify, numberErrors)
import Data.Data.GenRep.Doc (valueToDoc)

import Data.Dynamic hiding (typeOf)
import qualified Data.Data as D
import Control.DeepSeq (force)
import Control.Monad (void)
import qualified Control.Monad.Catch as MC
import Control.Monad.Trans (liftIO)
import Data.Char (isAlpha)
import Data.Maybe (catMaybes, maybe)

----------------------------------------------------------------------

data TaskChan 
    = TC 
        { logger    :: Logger
        , hoogledb  :: Maybe FilePath   -- for Hoogle searches
        , chan      :: Simple.TaskChan
        }

startGHCiServer :: [FilePath] -> Logger -> Maybe FilePath -> IO TaskChan
startGHCiServer searchpaths log dbname = do
    ch <- Simple.startGHCiServer searchpaths log
    return $ TC
            { logger    = log
            , hoogledb  = dbname
            , chan      = ch
            }

restart :: TaskChan -> IO ()
restart ch = do
    Simple.restartGHCiServer (chan ch)

---------------

showErr :: Language -> InterpreterError -> String
showErr lang (WontCompile l)   = unlines $ map errMsg l
showErr lang (UnknownError s)  = translate lang "Unknown error: " ++ s
showErr lang (NotAllowed s)    = translate lang "Not allowed: " ++ s
showErr lang (GhcException s)  = translate lang "GHC exception: " ++ s

----------------------------------------------------------------------

getCommand :: String -> (String, String)
getCommand (':':'?': (dropSpace -> Just x)) 
    = ("?", x)
getCommand (':': (span isAlpha -> (c@(_:_), dropSpace -> Just x)))
    = (c, x)
getCommand s
    = ("", s)

dropSpace :: String -> Maybe String
dropSpace (' ':y) = Just $ dropWhile (==' ') y
dropSpace "" = Just ""
dropSpace _ = Nothing


interp :: Bool -> Hash -> Language -> TaskChan -> FilePath -> String 
    -> Maybe (String -> Interpreter Result) -> IO Result
interp  verboseinterpreter (show -> idi) lang ch fn s@(getCommand -> (cmd, expr)) extraStep
    = force <$> case cmd of

        "?" -> hoogle expr

        "i" -> hoogleInfo expr

        c | c `elem` ["t","k",""]
           -> fmap (either (Error True . showErr lang) id)
          $ sendToServer (chan ch) fn
          $ case cmd of
              "t" ->
                do
                  xx <- typeOf expr
                  return $ ExprType False expr xx []
                `catchE`
                  (return . Error True . showErr lang)
              "k" ->
                do 
                  xx <- kindOf expr
                  return $ TypeKind expr xx []
                `catchE`
                  (return . Error True . showErr lang)
              "" ->
                (exprPpr expr) `catchE`
                  (\tyErr ->
                     let throwErr = MC.throwM tyErr
                     in (typeKind expr) `orElse` (maybe throwErr ($ expr) extraStep) `orElse` throwErr)

        _   ->  return $ force $ Error True $ 
                   translate lang "The" ++ " :" ++ cmd ++ " " ++ translate lang "command is not supported" ++ "."

 where
    mDb :: Maybe FilePath
    mDb = hoogledb ch

    exprType :: String -> Interpreter String
    exprType = typeOf

    exprPpr :: String -> Interpreter Result
    exprPpr expr = do
      ty <- exprType expr
      case specialize ty of
        Left err         -> return (Error True "Internal error during type checking. Sorry.")
        Right (ty',ty'') -> do
          result <- (pprData expr ty'') `orElseMaybe` (ppr expr ty')
          case result of
            Nothing -> do
              return $ Error False ("I don't know how to evaluate this expression but I can show its type: " ++ ty') -- or ty'' ??
            Just res ->
              return res

    pprData :: String -> String -> Interpreter (Maybe Result)
    pprData expr type_ =  do
      wd <- interpret ("wrapData (" ++ parens expr ++ " :: " ++ type_ ++")") (as :: WrapData)
      liftIO (pprintData idi type_ wd)

    ppr :: String -> String -> Interpreter (Maybe Result)
    ppr expr type_ = do
      dyn <- interpret ("toDyn (" ++ parens expr ++ " :: " ++ type_ ++")") (as :: Dynamic)
      liftIO (pprint idi dyn)

    typeKind :: String -> Interpreter Result
    typeKind expr = do
       k <- kindOf expr
       return $ TypeKind expr k []

    hoogle :: String -> IO  Result
    hoogle expr = maybe (noHoogle expr) (\db -> query db expr) mDb

    hoogleInfo :: String -> IO Result
    hoogleInfo expr = maybe (noHoogle expr) (\db -> queryInfo lang db expr) mDb

    noHoogle :: String -> IO Result
    noHoogle expr = return (noInfo expr)

    orElse :: Interpreter a -> Interpreter a -> Interpreter a
    orElse x y = x `catchE` \_ -> y

    orElseMaybe :: Interpreter (Maybe a) -> Interpreter (Maybe a) -> Interpreter (Maybe a)
    orElseMaybe x y = do
      res <- x `orElse` (return Nothing)
      case res of
        Just _ -> return res
        Nothing -> y

    catchE :: Interpreter a -> (InterpreterError -> Interpreter a) -> Interpreter a
    catchE = Simple.catchError_fixed

    noInfo :: String -> Result
    noInfo query = Message (translate lang "No info for " ++ query) Nothing

--------------------


pprintData :: String -> String -> WrapData -> IO (Maybe Result)
pprintData idi y (WrapData x)
  | D.dataTypeName (D.dataTypeOf x) == "Diagram" =
      return Nothing
  | otherwise = do
      a <- C.eval 1 700 x
      let ([p], es) = numberErrors [a]
      return . Just $ ExprType False (show $ valueToDoc p) y es


pprint :: String -> Dynamic -> IO (Maybe Result)
pprint idi d
    | Just x <- fromDynamic d = ff x
    | Just x <- fromDynamic d = ff $ showFunc (x :: Double -> Double)
    | Just x <- fromDynamic d = ff $ showFunc (x :: Double -> Integer)
    | Just x <- fromDynamic d = ff $ showFunc $ fromIntegral . fromEnum . (x :: Double -> Bool)
    | Just x <- fromDynamic d = ff $ showFunc $ fromIntegral . fromEnum . (x :: Double -> Ordering)
    | Just x <- fromDynamic d = ff $ showFunc_ (x :: Integer -> Double)
    | Just x <- fromDynamic d = ff $ showFunc_ (x :: Integer -> Integer)
    | Just x <- fromDynamic d = ff $ showFunc_ $ fromIntegral . fromEnum . (x :: Integer -> Bool)
    | Just x <- fromDynamic d = ff $ showFunc_ $ fromIntegral . fromEnum . (x :: Integer -> Ordering)
    | Just x <- fromDynamic d = ff $ displayArc' (x :: Double -> (Double, Double))
    | Just (f,g) <- fromDynamic d = ff $ displayArc' ((\x -> (f x, g x)) :: Double -> (Double, Double))
    | otherwise = return Nothing
 where
    ff = fmap g . render 10 (-16, -10) (16, 10) 5 2048 idi
    g (htm, err) = Just (Dia htm err)
    showFunc :: (RealFrac a, Real b) => (a -> b) -> Diagram
    showFunc = displayFun (-16,-10) (16,10)
    showFunc_ :: (Real b, Integral a) => (a -> b) -> Diagram
    showFunc_ = displayDiscreteFun (-16,-10) (16,10)
    displayArc' = displayArc (-16,-10) (16,10) (0,1) 

------------------------

wrap2 :: String -> String -> String
wrap2 a b = "WrapData2 " ++ parens a ++ " " ++ parens b

----------------

compareMistGen :: Language -> String -> WrapData2 -> String -> IO Result
compareMistGen lang idi (WrapData2 x y) goodsol
    | D.dataTypeName (D.dataTypeOf x) == "Diagram" 
    = return $ Message (translate lang "Can't decide the equality of diagrams (yet).") Nothing
compareMistGen lang idi (WrapData2 x y) goodsol = do
    (ans, a', b') <- C.compareData 0.8 0.2 700 x y
    return $ case ans of
        C.Yes -> Message (translate lang "Good solution! Another good solution:")
                          $ Just $ ExprType False goodsol "" []
        _ ->
            let x = case ans of
                    C.Maybe _  -> "I cannot decide whether this is a good solution:"
                    C.No       -> "Wrong solution:"
            in Message (translate lang x) $ Just $ showPair ans (a', mistify b')


---------------------------------

compareClearGen :: Language -> String -> WrapData2 -> IO Result
compareClearGen lang idi (WrapData2 x y)
    | D.dataTypeName (D.dataTypeOf x) == "Diagram"
    = return $ Message (translate lang "Can't decide the equality of diagrams (yet).") Nothing
compareClearGen lang idi (WrapData2 x y) = do
    (ans, a', b') <- C.compareData 0.8 0.2 700 x y
    return $ case ans of
--        C.Yes -> []
        _ -> showPair ans (a', b')


showPair :: C.Answer -> (GenericData, GenericData) -> Result
showPair x (a, b) = Comparison (show (valueToDoc a')) x (show (valueToDoc b')) es
  where ([a', b'], es) = numberErrors [a, b]



