{-# LANGUAGE DeriveGeneric #-}

module Models (
  TodoItem(..)
) where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)

data TodoItem = TodoItem {
  id :: String,
  content :: String,
  done :: Bool
} deriving (Generic, Show)
  
instance ToJSON TodoItem where
    toEncoding = genericToEncoding defaultOptions
  
instance FromJSON TodoItem