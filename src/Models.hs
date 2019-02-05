{-# LANGUAGE DeriveGeneric #-}

module Models (
  TodoItem(..)
) where

import           Data.Aeson   (FromJSON, ToJSON, defaultOptions,
                               genericToEncoding, toEncoding)
import           GHC.Generics

data TodoItem = TodoItem {
  id      :: Maybe String,
  content :: String,
  done    :: Bool
} deriving (Generic, Show)

instance ToJSON TodoItem where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON TodoItem
