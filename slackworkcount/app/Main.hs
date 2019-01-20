{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Either
import Data.Fixed
import Data.Maybe
import Data.String (fromString)
import Data.String.Conversions
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format (FormatTime, formatTime, parseTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.LocalTime
import GHC.Generics
import System.Environment (getEnv)
import System.Hclip
import qualified Web.Slack as Slack
import qualified Web.Slack.Api as Api
import qualified Web.Slack.Common as Api
import Web.Slack.Common
import Web.Slack.User

$(deriveToJSON defaultOptions ''SlackTimestamp)
$(deriveToJSON defaultOptions ''SlackMessageText)
$(deriveToJSON defaultOptions ''UserId)
$(deriveToJSON defaultOptions ''MessageType)
instance ToJSON Message

main :: IO ()
main = do
  token <- getEnv "SLACK_TOKEN"
  userNameEnv <- fromString <$> getEnv "SLACK_USERNAME" :: IO Text
  slackConfig <- Slack.mkSlackConfig (fromString token)
  a <- flip runReaderT slackConfig (Slack.apiTest Api.mkTestReq)
  users <- flip runReaderT slackConfig Slack.usersList
  userId <- return $ userId . head $ filter (\a -> (userName a) == userNameEnv) (listRspMembers (either (error . show) id users))
  return ()
