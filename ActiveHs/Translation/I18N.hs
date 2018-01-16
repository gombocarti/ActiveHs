module ActiveHs.Translation.I18N (
    I18N
  , translateParam1
  , translateParam1Str
  , mkI18N
  ) where

import qualified ActiveHs.Translation.Base as B
import           ActiveHs.Translation.Dictionary (hunDict, lookupDict)

import qualified Data.Text as T
import           Text.Printf (printf)

type I18N = B.Translation -> T.Text

translateParam1 :: I18N -> B.Translation -> T.Text -> T.Text
translateParam1 i18n trans arg = T.pack (printf (T.unpack $ i18n trans) arg)

translateParam1Str :: I18N -> B.Translation -> String -> T.Text
translateParam1Str i18n trans arg = T.pack (printf (T.unpack $ i18n trans) arg)

mkI18N :: B.Language -> I18N
mkI18N lang trans =
  B.languageCata
    (B.translationText trans)   -- En
    (B.translationCata          -- Hu
      (\_ident defaultText ->
         case lookupDict trans hunDict of
           Just translation -> translation
           Nothing -> defaultText)
      trans)
  lang
       
                  
