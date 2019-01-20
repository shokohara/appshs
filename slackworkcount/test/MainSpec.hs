{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (Result)
import Control.Exception (evaluate)
import Lib
import Web.Slack.Common
import Data.Aeson (eitherDecode, Value, Result)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteExp))
import TestLib
import Data.Aeson
import System.IO.Unsafe
import qualified Data.ByteString.Lazy.Char8 as C8

m = unsafePerformIO . C8.readFile $ "messages.json"

messages :: [Message]
messages = either (error . show) id (eitherDecode  m )

spec :: Spec
spec = do
  describe "aa" $ do
    it "bb" $ do
      messages `shouldBe` []
--      f (foldMap (error . show) ((fromJSON [jsonFile|/Users/sho/src/github.com/shokohara/appshs/slackworkcount/messages.json|] ) :: Result [Message])) `shouldBe` []
