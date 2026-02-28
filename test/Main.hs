module Main (main) where

import Network.QBittorrent.SimpleClient
import Test.Hspec

config :: QBConfig
config =
  (defaultConfig :: QBConfig)
    { host = "192.168.50.98"
    , credential = Credential "admin" "123456"
    }

main :: IO ()
main = hspec $ do
  describe "getTorrents" $ do
    it "returns torrent list successfully" $ do
      result <- initQBClient config
      case result of
        Left err -> expectationFailure $ "Login failed: " <> show err
        Right client -> do
          torrents <- client.getTorrents Nothing
          case torrents of
            Left err -> expectationFailure $ "getTorrents failed: " <> show err
            Right ts -> do
              putStrLn $ "Got " <> show (length ts) <> " torrents:"
              mapM_ (\t -> putStrLn $ "  - " <> show t.name <> " [" <> show t.state <> "]") ts
