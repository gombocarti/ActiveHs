module Test (testDefinition) where

import Smart
import ActiveHs.Base (WrapData2 (WrapData2))
import Lang
import Result
import Logger
import Qualify (qualify)
import Specialize (specialize)

import qualified Data.Data.Compare as C

import Data.List (intercalate)
import Control.Monad (forM)
import Control.Monad.Trans (liftIO)

---------------------------------------

testDefinition
    :: String
    -> Language
    -> TaskChan
    -> FilePath
    -> [String]
    -> [([String],String)]      -- test cases
    -> IO Result
testDefinition qualifier lang ch fn funnames testCases' = do
    logStrMsg 3 (logger ch) $ "test cases: " ++ show testCases'
    logStrMsg 3 (logger ch) $ "names to be qualified: " ++ show funnames

    let testCases = map snd testCases'
    case allRight $ map (qualify qualifier funnames) testCases of
        Left err -> do
            logStrMsg 3 (logger ch) $ "Error in qualification: " ++ err
            return $ Error True err
        Right expectedResults -> do
            logStrMsg 3 (logger ch) $ "Qualified expressions: " ++ show expectedResults
            inferenceResult <- runInterpreter ch lang fn $ do
                    tests <- forM (testCases `zip` expectedResults) $ \(actual, expected) -> do
                      tActual <- typeOf actual
                      tExpected <- typeOf expected
                      let [actualFixed, expectedFixed] = map fixType [(actual,tActual), (expected,tExpected)]
                      return $ wrapData2 actualFixed expectedFixed
                    let testSuiteCode = "[" ++ intercalate ", " tests ++ "]"
                    liftIO $ logStrMsg 3 (logger ch) $ "Test cases: " ++ testSuiteCode
                    interpret testSuiteCode (as :: [WrapData2])
            case inferenceResult of
              Success testSuite -> test lang (zip testSuite testCases)
              Failure err       -> return (Error False err)

  where
    fixType :: (String, String) -> String
    fixType (s,t) =
      case (specialize t) of
        Right (st,_) | t /= st -> unwords [s, "::", st]
        _ -> s

test :: Language -> [(WrapData2, String)] -> IO Result
test lang testSuite = foldM testsPassed (runTest lang) testSuite
  where
    testsPassed :: Result
    testsPassed = Message (translate lang "All test cases are completed.") Nothing

-- Nothing means test is passed.
-- Just result means test is failed and result holds the test case and reason.
runTest :: Language -> (WrapData2, String) -> IO (Maybe Result)
runTest lang ((WrapData2 actual expected), testCase) =
  toTestResult <$> compareClearGen lang actual expected

  where
    toTestResult :: (Result, C.Answer) -> Maybe Result
    toTestResult (_, C.Yes) = Nothing
    toTestResult (res, _)   = Just $ ShowFailedTestCase testCase res

------------------------------------

allRight :: [Either a b] -> Either a [b]
allRight x = case [s | Left s<-x] of
    (y:_) -> Left y
    []    -> Right [s | Right s<-x]

foldM :: Monad m => r -> (a -> m (Maybe r)) -> [a] -> m r
foldM end _ [] = return end
foldM end f (t:ts) = do
    x <- f t
    case x of
        Just r -> return r
        Nothing  -> foldM end f ts
