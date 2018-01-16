{-# LANGUAGE TemplateHaskell #-}

module ActiveHs.Translation.Entries where

import ActiveHs.Translation.Base (generateTranslationEntries, labels)

$(generateTranslationEntries labels)
