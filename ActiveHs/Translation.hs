module ActiveHs.Translation (
    module ActiveHs.Translation.Base
  , module ActiveHs.Translation.I18N
  , en
  , hu
  ) where

import           ActiveHs.Translation.Base (Language(En, Hu))
import           ActiveHs.Translation.I18N (I18N, mkI18N)

en :: I18N
en = mkI18N En

hu :: I18N
hu = mkI18N Hu
