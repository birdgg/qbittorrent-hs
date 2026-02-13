# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build all packages
cabal build all

# Build specific package
cabal build qbittorrent
cabal build effectful-qbittorrent

# Run GHCi for a specific package
cabal repl qbittorrent
cabal repl effectful-qbittorrent
```

## Project Structure

This is a mono-repo containing two packages:

```
qbittorrent-hs/
├── cabal.project                    # Multi-package configuration
├── qbittorrent/                     # Core library
│   ├── qbittorrent.cabal
│   └── src/Network/QBittorrent/
└── effectful-qbittorrent/           # Effectful integration
    ├── effectful-qbittorrent.cabal
    └── src/Effectful/
```

## Architecture

### qbittorrent (Core Library)

Haskell client library for the qBittorrent 5.0 Web API using servant-client.

**Note**: This library only supports qBittorrent 5.0+. It does not provide backwards compatibility with qBittorrent 4.x API.

#### Module Structure

- `Network.QBittorrent` - Main entry point, re-exports Client and API
- `Network.QBittorrent.API` - Servant API type definitions using `NamedRoutes`
- `Network.QBittorrent.Client` - `QBClient`, `newClient`, `runQB`, and `ClientM` API functions
- `Network.QBittorrent.SimpleClient` - Record-based API with `IO (Either QBClientError a)` return types
- `Network.QBittorrent.Types` - Core types (`QBConfig`, `QBClientError`, `QBResponseError`)
- `Network.QBittorrent.Types.*` - Domain types (Torrent, Filter, Form, Sync)

### effectful-qbittorrent

Effectful static effect bindings for the qBittorrent Web API.

#### Module Structure

- `Effectful.QBittorrent` - Static effect `QBittorrent`, `runQBittorrent`, and lifted operations

### Key Patterns

**Servant Generic Routes**: The API uses `NamedRoutes` with record-based route definitions:
```haskell
data QBittorrentRoutes mode = QBittorrentRoutes
  { auth :: mode :- "auth" :> NamedRoutes AuthRoutes
  , torrents :: mode :- "torrents" :> NamedRoutes TorrentsRoutes
  ...
  }
```

**Client Generation**: Uses `genericClient` to generate `ClientM` functions from route definitions.

**Effectful Static Effect**: Uses `Static WithSideEffects` dispatch for the QBittorrent effect.

## Language Extensions (GHC2021 base)

Key extensions enabled by default:
- `OverloadedRecordDot` - Record field access with `.`
- `NoFieldSelectors` - No generated field accessor functions
- `DuplicateRecordFields` - Same field names across types
- `DataKinds`, `TypeFamilies` - For servant type-level programming
