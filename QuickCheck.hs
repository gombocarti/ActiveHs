module QuickCheck where

import Smart
import ActiveHs.Base (WrapData2 (WrapData2), TestCase (TestCase))
import Lang
import Result
import Logger
import Qualify (qualify)
import Hash
import Specialize (specialize)

import Test.QuickCheck hiding (Result, Success, Failure)
import qualified Test.QuickCheck.Property as QC

import Data.Char (isLower)
import Data.List (intercalate)
import Control.Monad (join,forM,void)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar (newMVar, swapMVar, takeMVar)

---------------------------------------

quickCheck
    :: String
    -> Hash
    -> Language
    -> TaskChan
    -> FilePath
    -> String
    -> [String]
    -> [([String],String)]      -- test cases
    -> IO Result
quickCheck qualifier m5 lang ch fn ft funnames testcases = do
    logStrMsg 3 (logger ch) $ "test cases: " ++ show testcases
    logStrMsg 3 (logger ch) $ "names to be qualified: " ++ show funnames

    case allRight $ map (qualify qualifier funnames . snd) testcases of
        Left err -> do
            logStrMsg 3 (logger ch) $ "Error in qualification: " ++ err
            return $ Error True err
        Right s_ -> do
            logStrMsg 3 (logger ch) $ "Qualified expressions: " ++ show s_
            inferenceResult <- runInterpreter ch lang fn $ do
                    ss <- forM (testcases `zip` s_) $ \((v,s1),s2) -> do
                      ts1 <- typeOf s1
                      ts2 <- typeOf s2
                      let [x1,x2] = map fixType [(s1,ts1),(s2,ts2)]
                      return $ mkTestCase (v,x1,x2)
                    let ts = "[" ++ intercalate ", " ss ++ "]"
                    liftIO $ logStrMsg 3 (logger ch) $ "Test cases: " ++ ts
                    liftIO $ logStrMsg 3 (logger ch) "Before interpretation"
                    tests <- interpret ts (as :: [TestCase])
                    liftIO $ logStrMsg 3 (logger ch) "After interpretation"
                    return tests
            case inferenceResult of
              Success typedTests -> qcs lang (logger ch) typedTests
              Failure err        -> return $ Error False err

  where
    fixType (s,t) =
      case (specialize t) of
        Right (st,_) | t /= st -> unwords [s, "::", st]
        _ -> s

    mkTestCase (vars, s, s2)  
        = "TestCase (\\qcinner " 
        ++ unwords ["(" ++ v ++ ")" | v<-vars] 
        ++ " -> qcinner (" ++ showTr vars s ++ ", " ++ parens s ++ ", " ++ parens s2 ++ "))"

    showTr vars s = "unwords [" ++ intercalate ", " (map g $ words s) ++ "]"  -- !!!
     where

        vs = concatMap f vars

        f = filter (isLower . head) . words

        g x | x `elem` vs = {- "\"(\" ++ -} " show " ++ x -- ++ " ++ \")\""
        g x = show x

qcs :: Language -> Logger -> [TestCase] -> IO Result
qcs lang log = foldM (Message (translate lang "All test cases are completed.") Nothing) (qc lang log)

-- test = qc $ TestCase $ \f (QCNat x) (QCInt y) -> f (show x ++ " + " ++ show y, min 10 (x + y), x + y)
-- test' = qc $ TestCase $ \f (QCInt x) -> f ("sqr " ++ show x, min 10 (x*x), x*x)

qc :: Language -> Logger -> TestCase -> IO (Maybe Result)
qc lang log (TestCase p) = do
    v <- newMVar Nothing
    let ff (s, x, y)
            = QC.morallyDubiousIOProperty $ do
                r <- res
                if hasError [r]
                  then do 
                    void $ swapMVar v $ Just $ ShowFailedTestCase s r
                    return QC.failed
                  else return QC.succeeded
          where
            res :: IO Result
            res = do
                logStrMsg 4 log $ "compare: " ++ s
                compareClearGen lang x y
    _ <- quickCheckWithResult (stdArgs { chatty = False }) $ {- QC.noShrinking $ -} p ff
    takeMVar v

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


