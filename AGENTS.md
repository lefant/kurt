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

This is an older Haskell project that now uses Cabal through devenv as the supported local workflow:

- `kurt.cabal` declares the `kurt` executable and `kurt-test` test suite.
- The former Stack/Travis setup targeted GHC 7.8 and was removed from active tooling because current Stack releases no longer support that old Cabal/GHC combination cleanly.
- The Dockerfile is based on Ubuntu 14.04 and should be treated as historical unless intentionally modernizing it.

Use the project devenv shell for documented build and test dependencies (GHC, cabal-install, Python):

```sh
devenv shell
build
cabal v2-test
smoke
```

Equivalent direct Nix command:

```sh
nix shell nixpkgs#ghc nixpkgs#cabal-install -c cabal v2-build
```

If intentionally reproducing the historical Stack build, recover it from git history and use an old Stack release that still supports Cabal 1.18, or reintroduce Stack with a current resolver deliberately.

## Haskell unit/property testing

Run the Cabal-backed Haskell invariant suite with:

```sh
cabal v2-test
```

Or through devenv:

```sh
devenv shell test
```

This suite covers internal board, coordinate, game-state, chain, and hash invariants. It complements the black-box GTP regression suite; it does not replace client-facing GTP checks.

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

In an Amp orb, `.agents/setup` installs and caches the pinned devenv toolchain, builds Kurt, and runs both test suites. New login shells opened at the repository root already have that environment active, so `cabal`, `build`, and `smoke` can be run directly. Use `cabal v2-test` for unit tests; bare `test` is a shell builtin.

GTP is a stateful stdin/stdout protocol, so `scripts/kurt-gtp` is not itself an orb portal service. Drive it from the orb Terminal, use `scripts/kurt-remote-gtp` from a desktop GTP client when an SSH-reachable engine host is available, or use the repository's browser bridge.

See `docs/orb-access.md` before setting up interactive access to an orb. Amp provides the thread Terminal and Desktop panes, but not a direct SSH connection string. The documented tmate fallback is temporary, third-party, and unsuitable as a GTP transport.

Expected behavior:

- Protocol basics pass for direct engine startup and `scripts/kurt-gtp` wrapper startup.
- GTP responses start with `=` for accepted commands.
- `name` returns `kurt`.
- `protocol_version` returns `2`.
- `known_command` and `list_commands` cover core commands.
- `genmove` returns an in-board coordinate, `pass`, or `resign`.
- `quit` returns a normal empty success response and exits cleanly.
- Engine stdout remains GTP-clean; stderr diagnostics are tolerated.

## Browser bridge

`scripts/kurt-web` starts the dependency-free Python bridge in `web/bridge.py`. It serves the 9×9 UI and starts one isolated `scripts/kurt-gtp` engine per browser session. Browser requests never select an executable or command.

Run it locally with `scripts/kurt-web --port 8080`. In an Amp orb, use `amp orb services ensure`; `.amp/services.yaml` supervises the process and publishes the portal. Do not start it as an ad-hoc background process.

Run focused bridge tests with:

```sh
python3 -m unittest discover -s test -p 'test_web_bridge.py' -v
```

When changing the bridge, also run `cabal v2-test` and `scripts/gtp-regression`. Exercise the actual UI through the declared portal for interaction or appearance changes.

## Current caveats

- Compiler warnings are treated as errors in multiple modules and in `kurt.cabal` (`-Wall -Werror`). Newer GHC versions may fail on warnings that old CI did not see.
- `cmd_version` reports `0.0.3` while `kurt.cabal` says `0.0.4`.
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
