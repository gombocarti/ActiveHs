{-# LANGUAGE OverloadedStrings #-}
module ActiveHs.Hoogle (
      query
    , queryInfo
    ) where

import ActiveHs.Result (Result(SearchResults, Message))
import ActiveHs.Translation.I18N (I18N, translateParam1Str)
import ActiveHs.Translation.Entries (msg_Hoogle_NoInfo)

import qualified Hoogle as H

-------------------------

query :: FilePath -> String -> IO Result
query ch q = format <$> search' ch q

queryInfo :: I18N -> FilePath -> String -> IO Result
queryInfo i18n db q = do
    res <- search' db q
    case res of
      (r : _) -> return $ SearchResults False [H.targetDocs r]
      []      -> return $ Message (translateParam1Str i18n (msg_Hoogle_NoInfo "No info for %s") q) Nothing

search' :: FilePath -> String -> IO [H.Target]
search' dbname q = H.withDatabase dbname
                     (\db -> return $ H.searchDatabase db q)

format :: [H.Target] -> Result
format r = SearchResults (not $ null b) (map H.targetDocs a)
  where
    (a, b) = splitAt 10 r
