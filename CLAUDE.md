# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build the package
cabal build

# Run GHCi
cabal repl
```

## Architecture

This is a Haskell client library for the qBittorrent Web API using servant-client.

### Module Structure

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

## Language Extensions (GHC2021 base)

Key extensions enabled by default:
- `OverloadedRecordDot` - Record field access with `.`
- `NoFieldSelectors` - No generated field accessor functions
- `DuplicateRecordFields` - Same field names across types
- `DataKinds`, `TypeFamilies` - For servant type-level programming
