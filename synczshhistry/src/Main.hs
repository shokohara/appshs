module Main where

-- import Data.Text.IO
-- import Prelude hiding (readFile, putStrLn)
import Control.Exception (catch, throw)
import Data.ByteString (hGetLine, readFile)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Prelude hiding (putStrLn, readFile, lines)
import System.IO (IOMode(ReadMode), withFile, hPutStr, stderr)
import System.IO.Error (isEOFError)
import Data.ByteString.Char8
import Control.Monad

data History = History

path = "/Users/sho/zsh_history/histories"
main :: IO ()
main =
  readFile path >>= (\a -> forM_ (lines a) putStrLn)
  
--  withFile path ReadMode $
--  let loop f = catch
--        (hGetLine f >>= (\line -> putStrLn line >> loop f))
--        (\e -> if isEOFError e then return () else throw e)
--  in loop
--  withFile path ReadMode $
--    let loop f = catch (hGetLine f >>= (\line -> putStrLn line >> loop f)) (\e -> if isEOFError e then return () else (hPutStr stderr (show e) >>= (\_ -> return ())))
--    in loop
--  str <- readFile path
--  print $ formatTime defaultTimeLocale  "%c" $ posixSecondsToUTCTime 10
--  putStrLn str
