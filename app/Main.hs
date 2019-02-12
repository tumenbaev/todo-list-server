{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Ini               (Ini (..), lookupValue, readIniFile,
                                         readValue)
import           Data.Text              (Text (..), unpack)
import           Data.Text.Lazy         (pack)
import           Data.Text.Read         (decimal)
import           Lib
import           Models                 (TodoItem (..))
import           Web.Scotty             (delete, get, json, jsonData,
                                         middleware, param, post, put, scotty,
                                         text)

import           Data.Bson              (ObjectId, at, look)
import           Database.MongoDB       (Action, Document, Host (..), Pipe,
                                         PortID (PortNumber), Query, Value (..),
                                         access, at, auth, close, connect,
                                         deleteOne, find, host, insert, master,
                                         rest, save, select, selector, sort,
                                         (=:))
import           Network.Socket         (PortNumber)
import           Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..), simpleHeaders)

main = do
  ini <- readIniFile "config.ini"
  case getConf ini of
    Left error   -> print error
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
  scotty 4000 $ do
    middleware $ cors $ const $ Just CorsResourcePolicy {
      corsMethods=["GET","PUT","POST","DELETE"],
      corsOrigins=Nothing,
      corsRequestHeaders=simpleHeaders,
      corsExposedHeaders=Nothing,
      corsMaxAge=Nothing,
      corsVaryOrigin=False,
      corsRequireOrigin=False,
      corsIgnoreFailures=False
      }
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
    delete "/items/:id" $ do
      id <- param "id"
      liftIO $ accessDb pipe $ deleteOne $ select ["_id" =: (read id :: ObjectId)] "items"
      return ()

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
