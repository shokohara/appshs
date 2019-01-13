{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module AutoMerge where

import CliOpt
import qualified GitHub as GH
import qualified GitHub.Endpoints.PullRequests as GH
import GitHub.Data.Name
import qualified GitHub.Data.Request as R
import qualified Data.Text as T
import GitHub.Auth
import System.Environment
import Data.String (fromString)
import qualified Data.ByteString as B

main :: Program -> IO ()
main a = do
  mtoken <- lookupEnv "GITHUB_TOKEN" :: IO (Maybe String)
  mtoken2 <- return $  fromString <$> mtoken :: IO (Maybe B.ByteString)
--  ePrs <- GH.executeRequest' $ GH.pullRequestsFor' (fmap (OAuth . fromString) mtoken) (GH.mkOwnerName (T.pack(org a))) (GH.mkRepoName $ T.pack (repo a)) GH.optionsNoBase R.FetchAll
--  putStrLn $ show ePrs
  return ()

withAuth :: (GH.Auth -> IO ()) -> IO ()
withAuth action = do
    mtoken <- lookupEnv "GITHUB_TOKEN"
    case mtoken of
        Nothing    -> error "no GITHUB_TOKEN"
        Just token -> action (GH.OAuth $ fromString token)
