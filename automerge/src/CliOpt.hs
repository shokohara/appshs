{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module CliOpt where

import Options.Generic

data Program = AutoMerge { user :: String, org :: String, repo :: String } | ExampleSlack { org :: String } | Wc { a :: String } deriving Generic
instance ParseRecord Program
