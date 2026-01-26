# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build all packages
cabal build all

# Build specific package
cabal build qbittorrent
cabal build qbittorrent-effectful

# Run GHCi with a package
cabal repl qbittorrent
cabal repl qbittorrent-effectful
```

## Architecture

This is a Haskell client library for the qBittorrent Web API, organized as a multi-package Cabal project.

### Packages

- **qbittorrent**: Core library with pure servant-client bindings
- **qbittorrent-effectful**: Effectful effect wrapper with automatic session management

### Module Structure (qbittorrent-effectful)

- `Effectful.Qbittorrent` - Effectful effect with auto session management

### Module Structure (qbittorrent)

- `Network.QBittorrent` - Main entry point, re-exports Client and API
- `Network.QBittorrent.API` - Servant API type definitions using `NamedRoutes`
- `Network.QBittorrent.Client` - `ClientM` functions wrapping the API
- `Network.QBittorrent.Client.Auth` - Cookie jar and auth utilities
- `Network.QBittorrent.Types` - Core types (`QBConfig`, `QBError`)
- `Network.QBittorrent.Types.*` - Domain types (Torrent, Filter, Form, Sync)

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

**Effectful Integration**: The effectful package wraps `ClientM` operations in a `QBittorrent` effect with:
- Auto-login on first operation
- Cookie persistence via `TVar CookieJar`
- Auto-retry on 401/403 auth errors

## Language Extensions (GHC2021 base)

Key extensions enabled by default:
- `OverloadedRecordDot` - Record field access with `.`
- `NoFieldSelectors` - No generated field accessor functions
- `DuplicateRecordFields` - Same field names across types
- `DataKinds`, `TypeFamilies` - For servant type-level programming
