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
import Web.Slack (mkSlackConfig)
import Web.Slack.Common (UserId)
import Web.Slack.User
import Lib

main :: IO ()
main = do
  token <- getEnv "SLACK_TOKEN"
  userNameEnv <- fromString <$> getEnv "SLACK_USERNAME"
  slackConfig <- mkSlackConfig (fromString token)
  libMain slackConfig userNameEnv
