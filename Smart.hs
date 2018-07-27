{-# LANGUAGE ViewPatterns, PatternGuards, TypeApplications, RankNTypes,
             ScopedTypeVariables #-}
module Smart
    ( module Simple
    , startGHCiServer
    , restart
    , TaskChan (..)
    , infer
    , eval
    , evaluate
    , runInterpreter
    , compareClearGen
    , compareMistGen
    , Inferred(..)
    , wrapData2
    ) where

import Specialize
import Lang
import Result
import Logger
import Simple hiding (TaskChan, startGHCiServer)
import qualified Simple

import ActiveHs.Base (WrapData(..))
import Graphics.Diagrams (Diagram)
import Graphics.Diagrams.SVG (render)
import Graphics.Diagrams.FunctionGraphs (displayFun, displayDiscreteFun, displayArc)

import qualified Data.Data.Eval as C
import qualified Data.Data.Compare as C
import Data.Data.GenRep hiding (Error)
import Data.Data.GenRep.Functions (mistify, numberErrors)
import Data.Data.GenRep.Doc (valueToDoc)

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Data (Data)
import qualified Data.Data as D
import Data.Char (isAlpha)

----------------------------------------------------------------------

data Inferred a
  = Success a
  | Failure String

data TaskChan
    = TC
        { logger    :: Logger
        , chan      :: Simple.TaskChan
        }

startGHCiServer :: [FilePath] -> Logger -> Maybe FilePath -> Int -> IO TaskChan
startGHCiServer searchpaths log dbname reloadsPerGhciSession = do
    ch <- Simple.startGHCiServer searchpaths log reloadsPerGhciSession
    return $ TC
            { logger    = log
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

runInterpreter :: TaskChan -> Language -> FilePath -> Interpreter a -> IO (Inferred a)
runInterpreter ch lang fn action =
  either (Failure . showErr lang) Success <$> sendToServer (chan ch) fn action

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

evaluate :: TaskChan -> Language -> FilePath -> String -> IO Result
evaluate ch lang fn expr = do
  typingResult <- infer lang ch fn expr
  case typingResult of
    Left (TypeError err) -> return $ Error False err
    Right typedExpr -> eval lang fn typedExpr

eval :: Language -> FilePath -> InferenceResult -> IO Result
eval lang fn typedExpr =
  case typedExpr of
    DataInstance type_ (WrapData data_)->
      maybe (evalError type_) id <$> pprintData type_ data_
    Dynamic type_ dyn ->
      maybe (evalError type_) id <$> pprint dyn
    Type type_ expr_ ->
      return $ ExprType False expr_ type_ []
    Kind kind type_ ->
      return $ TypeKind type_ kind []

  where
    evalError :: String -> Result
    evalError t = Error False $ "I don't know how to evaluate this expression but I can show its type: " ++ t

infer :: Language -> TaskChan -> FilePath -> String -> IO (Either TypeError InferenceResult)
infer lang ch fn s@(getCommand -> (cmd, expr))
    = if cmd `elem` ["t","k",""]
      then fmap (either (Left . TypeError . showErr lang) id)
          $ sendToServer (chan ch) fn
          $ case cmd of
              "t" ->
                  (Right . flip Type expr <$> typeOf expr)
                  `catchE`
                  (return . Left . TypeError . showErr lang)
              "k" ->
                  (Right . flip Kind expr <$> kindOf expr)
                  `catchE`
                  (return . Left . TypeError . showErr lang)
              "" ->
                  (asDataOrDynamic expr)
                  `catchE`
                  (\_ -> do
                      t <- typeOf expr
                      return . Left . TypeError $ "I don't know how to evaluate this expression but I can show its type: " ++ t
                   `catchE`
                   (return . Left . TypeError . showErr lang)
                  )
              _ -> error "Smart.infer: impossible"
        else return $ Left $ TypeError $
               translate lang "The" ++ " :" ++ cmd ++ " " ++ translate lang "command is not supported" ++ "."

 where
    asDataOrDynamic :: String -> Interpreter (Either TypeError InferenceResult)
    asDataOrDynamic expr = do
      ty <- typeOf expr
      case specialize ty of
        Left err         -> return . Left . TypeError $ "Internal error during type checking. Sorry."
        Right (ty',ty'') -> do
          result <- asData expr ty''
          case result of
            Left _ ->
              asDynamic expr ty'
            _ ->
              return result

    asData :: String -> String -> Interpreter (Either TypeError InferenceResult)
    asData expr type_ =
      let expr' = "WrapData (" ++ parens expr ++ " :: " ++ type_ ++ ")"
          typeCheck = DataInstance type_ <$> interpret expr' (as :: WrapData)
      in (Right <$> typeCheck) `catchE` (return . Left . TypeError . showErr lang)

    asDynamic :: String -> String -> Interpreter (Either TypeError InferenceResult)
    asDynamic expr type_ =
      let typeCheck = Dynamic type_ <$> interpret ("toDyn (" ++ parens expr ++ " :: " ++ type_ ++")") (as :: Dynamic)
      in (Right <$> typeCheck) `catchE` (return . Left . TypeError . showErr lang)

    orElse :: Interpreter a -> Interpreter a -> Interpreter a
    orElse x y = x `catchE` \_ -> y

    catchE :: Interpreter a -> (InterpreterError -> Interpreter a) -> Interpreter a
    catchE = Simple.catchError_fixed

--------------------

pprintData :: Data a => String -> a -> IO (Maybe Result)
pprintData type_ x
  | D.dataTypeName (D.dataTypeOf x) == "Diagram" = pprint (toDyn x)
  | otherwise = do
      a <- C.eval 1 700 x
      let ([p], es) = numberErrors [a]
      return . Just $ ExprType False (show $ valueToDoc p) type_ es

pprint :: Dynamic -> IO (Maybe Result)
pprint d
    | Just x <- fromDynamic d = ff x
    | Just x <- fromDynamic d = ff $ showFunc (x :: Double -> Double)
    | Just x <- fromDynamic d = ff $ showFunc (x :: Double -> Integer)
    | Just x <- fromDynamic d = ff $ showFunc @Integer $ fromIntegral . fromEnum . (x :: Double -> Bool)
    | Just x <- fromDynamic d = ff $ showFunc @Integer $ fromIntegral . fromEnum . (x :: Double -> Ordering)
    | Just x <- fromDynamic d = ff $ showFunc_ (x :: Integer -> Double)
    | Just x <- fromDynamic d = ff $ showFunc_ (x :: Integer -> Integer)
    | Just x <- fromDynamic d = ff $ showFunc_ @Integer $ fromIntegral . fromEnum . (x :: Integer -> Bool)
    | Just x <- fromDynamic d = ff $ showFunc_ @Integer $ fromIntegral . fromEnum . (x :: Integer -> Ordering)
    | Just x <- fromDynamic d = ff $ displayArc' (x :: Double -> (Double, Double))
    | Just (f,g) <- fromDynamic d = ff $ displayArc' ((\x -> (f x, g x)) :: Double -> (Double, Double))
    | otherwise = return Nothing
 where
    ff = fmap g . render 10 (-16, -10) (16, 10) 5 2048 ""
    g (htm, err) = Just (Dia htm err)
    showFunc :: forall b a. (RealFrac a, Real b) => (a -> b) -> Diagram
    showFunc = displayFun (-16,-10) (16,10)
    showFunc_ :: forall b a. (Real b, Integral a) => (a -> b) -> Diagram
    showFunc_ = displayDiscreteFun (-16,-10) (16,10)
    displayArc' = displayArc (-16,-10) (16,10) (0,1)

------------------------

wrapData2 :: String -> String -> String
wrapData2 a b = unwords ["WrapData2", parens a, parens b]

----------------

compareMistGen :: Data a => Language -> String -> a -> a -> String -> IO Result
compareMistGen lang idi x y goodsol
    | D.dataTypeName (D.dataTypeOf x) == "Diagram"
    = return $ Message (translate lang "Can't decide the equality of diagrams (yet).") Nothing
compareMistGen lang idi x y goodsol = do
    (ans, a', b') <- C.compareData 0.8 0.2 700 x y
    return $ case ans of
        C.Yes -> Message (translate lang "Good solution! Another good solution:")
                          $ Just $ ExprType False goodsol "" []
        _ ->
            let x = case ans of
                    C.Maybe _  -> "I cannot decide whether this is a good solution:"
                    C.No       -> "Wrong solution:"
                    _          -> error "Smart.compareMistGen: impossible"
            in Message (translate lang x) $ Just $ showPair ans (a', mistify b')


---------------------------------

compareClearGen :: Data a => Language -> a -> a -> IO (Result, C.Answer)
compareClearGen lang x y
    | D.dataTypeName (D.dataTypeOf x) == "Diagram"
    = return $ (Message (translate lang "Can't decide the equality of diagrams (yet).") Nothing, C.Maybe 0)
compareClearGen _lang x y = do
    (ans, a', b') <- C.compareData 0.8 0.2 700 x y
    return $ case ans of
--        C.Yes -> []
        _ -> (showPair ans (a', b'), ans)


showPair :: C.Answer -> (GenericData, GenericData) -> Result
showPair x (a, b) = Comparison (show (valueToDoc a')) x (show (valueToDoc b')) es
  where ([a', b'], es) = numberErrors [a, b]
