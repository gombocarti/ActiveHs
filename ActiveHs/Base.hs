{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module ActiveHs.Base
    ( WrapData (WrapData)
    , WrapData2 (WrapData2)
    ) where

import Data.Data (Data, Typeable)

-------------------------

data WrapData
    = forall a. Data a 
    => WrapData a
        deriving (Typeable)

data WrapData2
    = forall a. Data a
    => WrapData2 a a
        deriving (Typeable)
