# qbittorrent-hs

Haskell client library for the qBittorrent Web API.

## Packages

- **qbittorrent**: Core API client with pure IO interface
- **qbittorrent-effectful**: Effectful bindings with automatic session management

## Installation

```cabal
build-depends: qbittorrent
```

For effectful users:

```cabal
build-depends: qbittorrent-effectful
```

## Quick Start

### Pure IO

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.QBittorrent

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let config = QBConfig
        { host = "localhost"
        , port = 8080
        , username = "admin"
        , password = "adminadmin"
        , useTLS = False
        }
  env <- mkClientEnv manager config

  -- Login
  result <- runClientM (login config) env
  case result of
    Right "Ok." -> putStrLn "Logged in!"
    _ -> putStrLn "Login failed"

  -- Get torrents
  torrents <- runClientM (getTorrents Nothing) env
  print torrents
```

### With Effectful

```haskell
import Effectful
import Effectful.Qbittorrent

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let config = QBConfig { ... }

  runEff
    . runQBittorrent manager config
    $ do
      torrents <- getTorrents Nothing
      liftIO $ print torrents
```

## API Coverage

- Authentication (login/logout)
- Torrent management (add, pause, resume, delete)
- Torrent info and files
- Tags management
- File/folder renaming
- Location management
- Sync API

## License

MIT
