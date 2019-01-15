{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Web.Slack as Slack
import qualified Web.Slack.Api as Api
import Control.Monad.Reader
-- import qualified Network.Slack.Api as Slack
import Data.String (fromString)
import System.Environment (getEnv)
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import Data.Aeson
import Data.Aeson.Casing
import Data.Either
import Data.String.Conversions
import Control.Monad
import Data.Time.LocalTime
import Data.Time.Format (parseTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Text (Text, pack, unpack)

data Message = Message {
  --messageClientMsgId :: String,
  messageText :: String, messageUser :: String, messageTs :: MyLocalTime } deriving (Show, Generic)
data Response = Response { responseOk :: Bool, responseMessages :: [Message], responseHasMore :: Bool, responseIsLimited :: Bool } deriving Generic

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Response where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype MyLocalTime = MyLocalTime { a :: LocalTime } deriving (Show, Generic)

instance FromJSON MyLocalTime where
  parseJSON = withText "LocalTime" $ \t ->
        case parseTime defaultTimeLocale "%FT%T%QZ" (unpack t) of
          Just d -> pure $ MyLocalTime d
          _      -> fail "could not parse ISO-8601 date"

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
  slackConfig <- Slack.mkSlackConfig token
--  res <- channels slackConfig
  flip runReaderT slackConfig (Slack.apiTest Api.mkTestReq)
--  forM_ (filter (\a -> messageUser a == "UELRGL7RR") res) print
--  print (ts <$> res)
  putStrLn "hello world"

