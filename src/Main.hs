{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import CliOpt
import Options.Generic
import qualified AutoMerge as AM
import qualified ExampleSlack as ES

main :: IO ()
main = do
  x <- getRecord "" :: IO Program
  exec x

exec :: Program -> IO ()
exec a@(AutoMerge _ _ _) = AM.main a
exec a@(ExampleSlack _) = ES.main a
