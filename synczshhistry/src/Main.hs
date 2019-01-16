module Main where

import Data.Time.Clock.POSIX
import Data.Time.Format

data History = History 
main :: IO ()
main = do
  str <- readFile "/Users/sho/zsh_history/histories"
  print $ formatTime defaultTimeLocale  "%c" $ posixSecondsToUTCTime 10
  putStrLn str
