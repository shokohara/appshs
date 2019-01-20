{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module TestLib(json, jsonFile) where

import Control.Exception (evaluate)
import Data.Aeson (eitherDecode, Value)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteExp))
import qualified Data.ByteString.Lazy.Char8 as C8
import System.IO.Unsafe
import Web.Slack.Common
import Lib

json :: QuasiQuoter
json = QuasiQuoter { quoteExp = buildJSONExp . parseExp }

jsonFile :: QuasiQuoter
jsonFile = QuasiQuoter { quoteExp = buildJSONExp . parseExp2 . unsafePerformIO . C8.readFile }

parseExp :: String -> Value
parseExp str =
    let result = eitherDecode (C8.pack str) in
    case result of
        Left err -> error err
        Right json -> json

parseExp2 :: C8.ByteString -> Value
parseExp2 s =
    let result = eitherDecode s in
    case result of
        Left err -> error err
        Right json -> json

buildJSONExp :: Value -> Q Exp
buildJSONExp value = [e| value |]
