# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
cabal build all
cabal repl
```

## Project Structure

```
qbittorrent-hs/
├── cabal.project
├── qbittorrent.cabal
└── src/Network/QBittorrent/
    ├── API/          # Servant route definitions (Auth, Torrents, App, Log, Sync, Transfer)
    ├── Types/        # Domain types (Torrent, Filter, Form, Sync, etc.)
    ├── API.hs        # Top-level API type combining all route groups
    ├── Client.hs     # QBClient, newClient, runQB, ClientM API functions
    ├── SimpleClient.hs  # Record-based API with IO (Either QBClientError a) return types
    └── Types.hs      # Core types (QBConfig, QBClientError, QBResponseError)
```

## Architecture

Haskell client library for the qBittorrent 5.0 Web API using servant-client.

Only supports qBittorrent 5.0+. No backwards compatibility with 4.x API.

### Key Patterns

**Servant Generic Routes**: Uses `NamedRoutes` with record-based route definitions:
```haskell
data QBittorrentRoutes mode = QBittorrentRoutes
  { auth :: mode :- "auth" :> NamedRoutes AuthRoutes
  , torrents :: mode :- "torrents" :> NamedRoutes TorrentsRoutes
  ...
  }
```

**Client Generation**: Uses `genericClient` to generate `ClientM` functions from route definitions.

**Two API Styles**:
- `Client` — Low-level `ClientM` monad, composable
- `SimpleClient` — High-level `IO (Either QBClientError a)`, auto-login

## Language Extensions (GHC2021 base)

Key extensions enabled by default:
- `OverloadedRecordDot`, `NoFieldSelectors`, `DuplicateRecordFields`
- `DataKinds`, `TypeFamilies` — For servant type-level programming
