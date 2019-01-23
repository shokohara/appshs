{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

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
import Data.Aeson.DeriveNoPrefix

$(deriveToJsonNoTypeNamePrefix ''SlackTimestamp)
$(deriveToJsonNoTypeNamePrefix ''SlackMessageText)
$(deriveToJsonNoTypeNamePrefix ''UserId)
$(deriveToJsonNoTypeNamePrefix ''MessageType)
$(deriveToJsonNoTypeNamePrefix ''Message)

libMain :: (Slack.HasManager env, Slack.HasToken env) => env -> Text -> IO ()
libMain slackConfig userNameEnv = do
  users <- flip runReaderT slackConfig Slack.usersList
  userId <- return $ userId . head $ filter (\a -> (userName a) == userNameEnv) (listRspMembers (either (error . show) id users))
  _ <- print userId
  c <- messages slackConfig "C04L83KET"
--  _ <- print c
--  LC.writeFile "messages.json" $ encode $ c
  myMessages <- return $ filter (\a -> Just userId == messageUser a) c
--  _ <- print myMessages
--  return ()
--  LC.putStrLn $ encodePretty $ myMessages
  writeFile "out" . show . head $ f myMessages
  setClipboard . show . head . f $ myMessages

messages slackConfig channel = do
  b <- flip runReaderT slackConfig (Slack.channelsHistory $ Api.mkHistoryReq channel)
  return $ historyRspMessages (either (error . show) id b)

data Output = Output { workTime :: Maybe WorkTime, note :: String } deriving Eq
data WorkTime = WorkTime {start :: UTCTime, rest :: NominalDiffTime, end :: UTCTime, total :: NominalDiffTime } deriving Eq

instance Show Output where
  show Output{..} = maybe "\t\t\t" show workTime <> "\t" <> note

instance Show WorkTime where
  show WorkTime{..} = format start <> "\t" <> format2 rest <> "\t" <> format end <> "\t" <> format2 total

format :: UTCTime -> String
format = formatTime defaultTimeLocale "%X" . (utcToLocalTime $ hoursToTimeZone 9)

format2 :: NominalDiffTime -> String
format2 = formatTime defaultTimeLocale "%X" . posixSecondsToUTCTime

f :: [Message] -> [Output]
-- f a = (\b -> (b, (secondsToNominalDiffTime (1 :: Pico)), b)) <$> filter (\a -> (==) "open" . unSlackMessageText . messageText )
f a = [Output (Just w) ""]
  where
    t :: UTCTime
    t = slackTimestampTime . messageTs . head $ filter ((== "back") . unSlackMessageText . messageText) a
    w = WorkTime t (diffUTCTime t t) t (diffUTCTime t t)
