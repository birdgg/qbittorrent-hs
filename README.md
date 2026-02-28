# qbittorrent-hs

Haskell client library for the qBittorrent Web API.

**Requires qBittorrent 5.0 or later.** This library does not support qBittorrent 4.x.

## Installation

```cabal
build-depends: qbittorrent
```

## Quick Start

### Simple Client (Recommended)

The simplest way to use the library. Provides a record-based API with `IO (Either QBClientError a)` return types and automatic session management:

```haskell
import Network.QBittorrent.SimpleClient qualified as QB

main :: IO ()
main = do
  result <- QB.initQBClient QB.defaultConfig
  case result of
    Left err -> print err
    Right client -> do
      -- Use record fields directly
      torrents <- client.getTorrents Nothing
      print torrents

      version <- client.getVersion
      print version
```

### With ReaderT (Recommended for Applications)

For larger applications, use `ReaderT` to pass the client through your app:

```haskell
import Control.Monad.Reader
import Network.QBittorrent.SimpleClient qualified as QB

type App a = ReaderT QB.Client IO a

runApp :: QB.QBConfig -> App a -> IO a
runApp config action = do
  Right client <- QB.initQBClient config
  runReaderT action client

myApp :: App ()
myApp = do
  client <- ask
  liftIO $ do
    torrents <- client.getTorrents Nothing
    print torrents

main :: IO ()
main = runApp QB.defaultConfig myApp
```

### Advanced Usage (ClientM)

For more control or to compose multiple API calls in `ClientM`:

```haskell
import Network.QBittorrent

main :: IO ()
main = do
  let config = defaultConfig
        { host = "localhost"
        , port = 8080
        , username = "admin"
        , password = "adminadmin"
        }
  client <- initQBClient config

  -- Login
  result <- runQB client (login config)
  case result of
    Right "Ok." -> putStrLn "Logged in!"
    _ -> putStrLn "Login failed"

  -- Get torrents (session cookie is managed automatically)
  torrents <- runQB client (getTorrents Nothing)
  print torrents
```

For custom HTTP manager settings, use `initQBClientWith`:

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.QBittorrent

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  client <- initQBClientWith manager config
  -- ...
```

### Multiple Clients with Shared Manager

When connecting to multiple qBittorrent instances, share the HTTP manager for better performance:

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.QBittorrent.SimpleClient qualified as QB

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings

  Right client1 <- QB.initQBClientWith manager config1
  Right client2 <- QB.initQBClientWith manager config2

  -- Both clients share the same connection pool
  torrents1 <- client1.getTorrents Nothing
  torrents2 <- client2.getTorrents Nothing
  print (torrents1, torrents2)
```