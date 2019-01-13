{-# LANGUAGE OverloadedStrings #-}
module Wc where

import qualified GitHub as GH
import CliOpt

main a = do
  
  putStrLn (org a)
