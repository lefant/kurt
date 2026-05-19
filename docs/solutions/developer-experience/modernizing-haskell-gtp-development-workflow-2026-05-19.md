---
title: "Modernizing Haskell GTP development workflow"
date: 2026-05-19
category: developer-experience
module: "Kurt development workflow"
problem_type: developer_experience
component: development_workflow
severity: medium
applies_when:
  - "Reviving an older Haskell Cabal or Stack project on a modern toolchain"
  - "Connecting a stdin/stdout GTP engine to remote GUI clients over SSH"
  - "Adding agent-facing project guidance and smoke tests to a legacy repository"
tags: [haskell, cabal, devenv, gtp, sabaki, ssh, smoke-test]
---

# Modernizing Haskell GTP development workflow

## Context

Kurt is an older Haskell Computer Go engine that speaks Go Text Protocol (GTP) over stdin/stdout. The repository had enough historical metadata to understand the project, but a fresh agent or Mac GUI client workflow needed several missing pieces: agent instructions, reproducible development dependencies, a modern build path, a GTP-safe wrapper, and clean `quit` behavior.

The historical Stack setup pinned `lts-2.14` and GHC 7.8.4. Current Stack releases can install that compiler, but then fail because they no longer support the Cabal library bundled with GHC 7.8.4. Modern Cabal with GHC 9.10.3 could build the project after small compatibility fixes.

A session-history search found no prior relevant attempts for this problem.

## Guidance

When reviving a legacy Haskell GTP engine for current agent and GUI workflows, solve the workflow as a thin compatibility layer rather than rewriting the engine.

1. Document the repository for agents.
   - Add `AGENTS.md` with project structure, build commands, smoke-test commands, caveats, and GTP expectations.
   - Include specific notes about stdout cleanliness because GTP clients parse stdout as protocol data.

2. Make Cabal metadata explicit enough for modern tooling.
   - Raise `cabal-version` enough for fields already used by the package.
   - Add all local modules to `other-modules` so modern Cabal does not fail with `-Wmissing-home-modules`.
   - Replace insecure `git://` repository URLs with `https://`.

   ```cabal
   cabal-version:  >= 1.10

   executable kurt
     main-is:        kurt.hs
     hs-source-dirs: src
     other-modules:  Data.Goban.GameState
                   , Data.Goban.Incremental
                   , Data.Goban.Types
                   , Data.Goban.Utils
                   , Data.Goban.ZobristHash
                   , Data.Tree.UCT
                   , Data.Tree.UCT.GameTree
                   , Debug.TraceOrId
                   , Kurt.Config
                   , Kurt.GoEngine
                   , Kurt.MainLoop
                   , Network.GoTextProtocol2.Server.Parser
                   , Network.GoTextProtocol2.Types
   ```

3. Keep modern-GHC code fixes small and behavior-preserving.
   - Add explicit `NFData` instances instead of relying on old empty instances whose default methods no longer compile cleanly with newer `deepseq`.
   - Replace deprecated `withSystemRandom` with `createSystemSeed` from `mwc-random`.
   - Make non-exhaustive pattern bindings explicit with an error path that should be unreachable by construction.

   ```haskell
   instance NFData Move where
       rnf (Move stone) = rnf stone
       rnf (Pass color) = rnf color
       rnf (Resign color) = rnf color
   ```

4. Put development dependencies in `devenv.nix`.
   - Include `ghc`, `cabal-install`, and `stack` to document both the modern build path and historical tooling.
   - Add scripts for the verified build and GTP smoke test.

   ```nix
   { pkgs, ... }:

   {
     packages = [
       pkgs.ghc
       pkgs.cabal-install
       pkgs.stack
     ];

     scripts.build.exec = "cabal v2-build";
     scripts.smoke.exec = ''
       printf 'name\nprotocol_version\nquit\n' | scripts/kurt-gtp
     '';
   }
   ```

5. Use SSH directly for Mac GTP clients.
   - GTP is stdin/stdout, so `ssh host /path/to/wrapper` is already a protocol bridge.
   - In Sabaki, configure `path` as `ssh` and `args` as the host plus wrapper path.

   ```json
   {
     "name": "kurt on lefant-kurt",
     "path": "ssh",
     "args": "lefant-kurt.exe.xyz /home/exedev/git/lefant/kurt/scripts/kurt-gtp"
   }
   ```

6. Use a wrapper script for GUI clients.
   - The wrapper centralizes the binary lookup.
   - If it must build first, redirect build output away from stdout so GTP clients do not see non-protocol text.
   - Do not assume interactive SSH has the same PATH as the agent shell; handle missing `cabal` by using an existing build artifact or a Nix fallback.

   ```sh
   #!/usr/bin/env bash
   set -euo pipefail

   cd "$(dirname "$0")/.."

   if command -v cabal >/dev/null 2>&1; then
     cabal v2-build >/dev/null
     exec "$(cabal list-bin kurt)" +RTS -N1
   fi

   for bin in ./dist-newstyle/build/*/ghc-*/kurt-*/x/kurt/build/kurt/kurt; do
     if [[ -x "$bin" ]]; then
       exec "$bin" +RTS -N1
     fi
   done
   ```

7. Make `quit` a clean GTP exit.
   - Do not implement `quit` by throwing `error "bye!"`; that prints a stack trace and exits nonzero.
   - Return an empty success response and stop the loop.

   ```haskell
   Just (_, handler) ->
       if cmd == "quit" || cmd == "kgs-game_over"
       then do
         putStrLn $ "=" ++ outputIdOrBlank maybeId
         newLineFlush
       else do
         result <- handler args oldState
         -- continue normal command handling
   ```

   This is a minimal compatibility patch. A larger cleanup could encode loop termination in the command result type instead of special-casing command names in the loop.

## Why This Matters

Legacy engine projects often fail at the workflow boundary before the domain logic can be tested. Modernizing the build metadata, documenting the environment, and adding a GTP-safe wrapper makes the engine usable from agents, CLI smoke tests, and Mac GUI clients without changing search or rules logic.

Clean stdout is especially important for GTP. Build logs, shell banners, or crash traces on stdout can corrupt the protocol stream. A wrapper script and clean `quit` handling keep GUI clients such as Sabaki from seeing confusing non-GTP output.

## When to Apply

- When a Haskell project has historical Stack or Cabal config but no current development shell.
- When modern Cabal reports missing home modules for an old executable-only package.
- When an engine protocol uses stdin/stdout and needs to be driven remotely from a desktop GUI.
- When a protocol `quit` command exits by exception rather than returning a normal protocol response.

## Examples

### Smoke test over SSH from a Mac

```sh
printf 'name\nprotocol_version\nquit\n' \
  | ssh lefant-kurt.exe.xyz /home/exedev/git/lefant/kurt/scripts/kurt-gtp
```

Expected output:

```text
= kurt

= 2

=
```

Expected exit code: `0`.

### Devenv build and smoke commands

```sh
devenv shell build
devenv shell smoke
```

The smoke command should verify `name`, `protocol_version`, a cheap `genmove`, and clean `quit` behavior.

## Related

- No existing `docs/solutions/` entries were present, so no overlap or refresh candidate was found.
- Related files: `AGENTS.md`, `devenv.nix`, `scripts/kurt-gtp`, `kurt.cabal`, `src/Kurt/MainLoop.hs`, `src/Kurt/GoEngine.hs`, `src/Data/Goban/Types.hs`, `src/Data/Tree/UCT/GameTree.hs`.
