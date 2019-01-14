{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Network.Slack.Api as Slack
import Network.Slack.Request
import Data.String (fromString)
import System.Environment (getEnv)
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import Data.Aeson
import Data.Aeson.Casing
import Data.Either
import Data.String.Conversions
import Control.Monad

data Message = Message {
  --messageClientMsgId :: String,
  messageText :: String, messageUser :: String, messageTs :: String } deriving (Show, Generic)
data Response = Response { responseOk :: Bool, responseMessages :: [Message], responseHasMore :: Bool, responseIsLimited :: Bool } deriving Generic

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Response where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

token :: IO Token
token = fromString <$> getEnv "TOKEN"

channels :: Token -> IO [Message]
channels token = do
  res <- Slack.channelsHistory token [("channel", "C04L83KET")]
  putStrLn (cs $ g res)
  return . responseMessages . b . g $ res
  where
    g :: Slack.SlackResponse -> L.ByteString
    g a = case a of
      Slack.InvalidEndpoint -> error ""
      Slack.Success a -> a
    b :: L.ByteString -> Response
    b = either error id . eitherDecode


main :: IO ()
main = do
  t <- token
  res <- channels t
  forM_ (filter (\a -> messageUser a == "UELRGL7RR") res) print
--  print (ts <$> res)
  putStrLn "hello world"

