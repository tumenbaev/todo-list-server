{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import Data.Ini (readIniFile, lookupValue, readValue, Ini(..))
import Data.Text (Text(..), unpack)
import Data.Text.Read (decimal)
import Control.Monad.Trans.Except (ExceptT(..))

import Database.MongoDB (
  Action, Document, Document, Value, Pipe, Query, access, close, connect, delete,
  exclude, find, host, insertMany, master, project, rest, readHostPort, auth,
  select, sort, (=:), Host(..), PortID(PortNumber))
import Network.Socket (PortNumber)

runQuery :: Pipe -> Query -> IO [Document]
runQuery pipe query = access pipe master "maverick" (find query >>= rest) 

runAction :: (MonadIO m) => Pipe -> Action m a -> m a
runAction pipe = access pipe master "maverick"

main = do
  ini <- readIniFile "config.ini"
  case getConf ini of
    Left error -> print error
    Right config -> startServer config


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
  runAction pipe $ auth user pass
  scotty 3000 $ do
    get "/:word" $ do
      -- beam <- param "word"
      -- html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
      res <- liftIO $ runQuery pipe (select [] "items")
      text $ T.pack $ show res
