{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Lib
import Models (TodoItem(..))
import Web.Scotty (scotty, get, text, json)
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import Data.Ini (readIniFile, lookupValue, readValue, Ini(..))
import Data.Text (Text(..), unpack)
import Data.Text.Read (decimal)
import Control.Monad.Trans.Except (ExceptT(..))

import Data.Bson (typed, valueAt)
import Database.MongoDB (
  Action, Document, Value, Pipe, Query, access, close, connect, delete,
  exclude, find, host, insertMany, master, project, rest, readHostPort, auth,
  select, sort, (=:), Host(..), PortID(PortNumber))
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
      json $ getItem <$> res

getItem :: Document -> TodoItem
getItem doc = TodoItem id content done where
  id = show $ valueAt "_id" doc
  content = typed $ valueAt "content" doc
  done = typed $ valueAt "done" doc  
