{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Prelude hiding (lookup)
import Lib
import Models (TodoItem(..))
import Web.Scotty (scotty, get, post, put, text, json, jsonData)
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Data.Text.Lazy (pack)
import Data.Ini (readIniFile, lookupValue, readValue, Ini(..))
import Data.Text (Text(..), unpack)
import Data.Text.Read (decimal)
import Control.Monad.Trans.Except (ExceptT(..))

import Data.Bson (typed, valueAt, at, look, lookup, ObjectId)
import Database.MongoDB (
  Action, Document, Value(..), Pipe, Query, access, close, connect, delete, insert, save,
  exclude, find, host, insertMany, master, project, rest, readHostPort, auth, at,
  select, sort, (=:), (!?), Host(..), PortID(PortNumber))
import Network.Socket (PortNumber)

main = do
  ini <- readIniFile "config.ini"
  case getConf ini of
    Left error -> print error
    Right config -> startServer config

accessDb :: MonadIO m => Pipe -> Action m a -> m a
accessDb pipe = access pipe master "maverick"
    
getConf :: Either String Ini -> Either String (String, PortNumber, Text, Text)
getConf eitherIni = do
  ini <- eitherIni
  host <- lookupValue "SERVER" "hostname" ini
  port <- readValue "SERVER" "port" decimal ini
  user <- lookupValue "AUTH" "user" ini
  pass <- lookupValue "AUTH" "pass" ini
  pure (unpack host, port, user, pass)

startServer :: (String, PortNumber, Text, Text) -> IO ()
startServer (host, port, user, pass) = do
  pipe <- connect $ Host host $ PortNumber port
  accessDb pipe $ auth user pass
  scotty 3000 $ do
    get "/items" $ do
      res <- liftIO $ accessDb pipe $ find (select [] "items") >>= rest
      json $ documentToItem <$> res
    post "/items" $ do
      item <- jsonData
      res <- liftIO $ accessDb pipe $ save "items" $ itemToDocument item
      json item
    put "/items" $ do
      item <- jsonData
      newId <- liftIO $ accessDb pipe $ insert "items" $ itemToDocument item
      text $ pack $ show newId

documentToItem :: Document -> TodoItem
documentToItem doc = TodoItem id content done where
  id = fmap show (look "_id" doc)
  content = at "content" doc
  done = at "done" doc

itemToDocument :: TodoItem -> Document
itemToDocument (TodoItem Nothing content done) = [
  "content" =: content,
  "done" =: done
  ]
itemToDocument (TodoItem (Just id) content done) = [
  "_id" =: (read id :: ObjectId),
  "content" =: content,
  "done" =: done
  ]
