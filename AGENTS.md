# AGENTS.md

## Project overview

Kurt is a Haskell Computer Go engine. It runs as a Go Text Protocol (GTP) server on stdin/stdout so clients such as GoGui, KGS GTP, or simple shell scripts can drive it.

Key areas:

- `src/kurt.hs` is the executable entry point. It parses command-line options from `Kurt.Config` and starts the main loop.
- `src/Kurt/MainLoop.hs` implements the GTP command loop and command handlers.
- `src/Kurt/GoEngine.hs` contains move generation, UCT/RAVE search, playout simulation, and engine state updates.
- `src/Kurt/Config.hs` defines runtime knobs such as board size, komi, max playouts, max time, threads, and heuristic weights.
- `src/Data/Goban/*` contains board state, rules, scoring, incremental board data structures, utilities, and Zobrist hashing.
- `src/Data/Tree/UCT/*` contains generic UCT tree support and move-node/rave-map types.
- `src/Network/GoTextProtocol2/*` contains GTP parsing and protocol types.

## Build system

This is an older Stack/Cabal Haskell project:

- `stack.yaml` pins `resolver: lts-2.14` and `rosezipper-0.1`.
- `kurt.cabal` declares the `kurt` executable only.
- Historical CI used GHC 7.8 and `stack --skip-ghc-check build`.
- The Dockerfile is based on Ubuntu 14.04 and should be treated as historical unless intentionally modernizing it.

Historical builds used Stack, but current Stack releases no longer support the Cabal library bundled with GHC 7.8. Use the project devenv shell for documented build and test dependencies (GHC, cabal-install, Python, Stack):

```sh
devenv shell
build
```

Equivalent direct Nix command:

```sh
nix shell nixpkgs#ghc nixpkgs#cabal-install -c cabal v2-build
```

If intentionally reproducing the historical build, use an old Stack release that still supports Cabal 1.18, or update the resolver/toolchain deliberately.

## Smoke testing

`scripts/kurt-gtp` runs the engine for GTP clients. When `cabal` is available, it builds with `cabal v2-build` while keeping build output off stdout, then runs the path reported by `cabal list-bin kurt`. Without `cabal`, it uses an existing Cabal build artifact or falls back to `nix shell`.

Run the local GTP regression suite:

```sh
scripts/gtp-regression
```

Or through devenv:

```sh
devenv shell smoke
```

Expected behavior:

- Protocol basics pass for direct engine startup and `scripts/kurt-gtp` wrapper startup.
- GTP responses start with `=` for accepted commands.
- `name` returns `kurt`.
- `protocol_version` returns `2`.
- `known_command` and `list_commands` cover core commands.
- `genmove` returns an in-board coordinate, `pass`, or `resign`.
- `quit` returns a normal empty success response and exits cleanly.
- Engine stdout remains GTP-clean; stderr diagnostics are tolerated.

## Current caveats

- Compiler warnings are treated as errors in multiple modules and in `kurt.cabal` (`-Wall -Werror`). Newer GHC versions may fail on warnings that old CI did not see.
- `cmd_version` reports `0.0.3` while `kurt.cabal` says `0.0.4`.
- The only file under `test/` imports modules that are not present in this checkout (`Data.Goban.Goban`, `Data.Goban.STVector`) and is not wired into `kurt.cabal`. Treat it as stale until repaired.
- The parser lowercases the entire input before parsing; be careful when adding commands or arguments where case matters.
- GTP coordinates skip the letter `I` by design.

## Documented Solutions

`docs/solutions/` contains documented solutions to past problems and workflow learnings, organized by category with YAML frontmatter (`module`, `tags`, `problem_type`). Relevant when implementing or debugging in documented areas.

## Development guidelines

- Preserve the stdin/stdout GTP contract. Do not print diagnostics to stdout from command handlers; use stderr or tracing that is explicitly disabled/enabled.
- Keep GTP command additions in sync between `commandargparserlist` and `commandHandlers` in `Kurt.MainLoop`.
- Keep board rule changes covered by small deterministic command sequences or focused unit tests where possible.
- Prefer small, behavior-preserving modernization steps when fixing compilation on a newer toolchain.
- Avoid broad rewrites of UCT/search code while solving build or smoke-test failures.
