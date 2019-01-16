{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Web.Slack as Slack
import qualified Web.Slack.Api as Api
import qualified Web.Slack.Common as Api
import Web.Slack.Common
import Web.Slack.User
import Control.Monad.Reader
import Data.String (fromString)
import System.Environment (getEnv)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson
import Data.Aeson.Casing
import Data.Either
import Data.String.Conversions
import Control.Monad
import Data.Time.LocalTime
import Data.Time.Format (parseTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Text (Text, pack, unpack)
import Data.Aeson.Encode.Pretty

$(deriveToJSON defaultOptions ''SlackTimestamp)
$(deriveToJSON defaultOptions ''SlackMessageText)
$(deriveToJSON defaultOptions ''UserId)
$(deriveToJSON defaultOptions ''MessageType)
instance ToJSON Message

main :: IO ()
main = do
  token <- getEnv "TOKEN"
  userNameEnv <- fromString <$> getEnv "SLACK_USERNAME" :: IO Text
  slackConfig <- Slack.mkSlackConfig (fromString token)
  a <- flip runReaderT slackConfig (Slack.apiTest Api.mkTestReq)
  users <- flip runReaderT slackConfig Slack.usersList
  userId <- return $ userId . head $ filter (\a -> (userName a) == userNameEnv) (listRspMembers (either (error . show) id users))
  b <- flip runReaderT slackConfig (Slack.channelsHistory $ Api.mkHistoryReq "C04L83KET")
  myMessages <- return $ filter (\a -> Just userId == messageUser a) $ historyRspMessages (either (error . show) id b)
  LC.putStrLn $ encodePretty $ myMessages
