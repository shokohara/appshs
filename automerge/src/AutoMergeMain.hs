{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoMergeMain where

import qualified AutoMerge as AM
import CliOpt
import qualified ExampleSlack as ES
import Options.Generic
import qualified Wc as W

main :: IO ()
main = do
  x <- getRecord "" :: IO Program
  exec x

exec :: Program -> IO ()
exec a@(AutoMerge _ _ _) = AM.main a
exec a@(ExampleSlack _) = ES.main a
exec a@(Wc _) = W.main a
