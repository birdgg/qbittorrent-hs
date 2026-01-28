# qbittorrent-hs

Haskell client library for the qBittorrent Web API.

**Requires qBittorrent 5.0 or later.** This library does not support qBittorrent 4.x.

## Installation

```cabal
build-depends: qbittorrent
```

## Quick Start

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

## API Coverage

- Authentication (login/logout)
- Torrent management (add, stop, start, delete)
- Torrent info and files
- Tags management
- File/folder renaming
- Location management
- Sync API

## License

MIT
