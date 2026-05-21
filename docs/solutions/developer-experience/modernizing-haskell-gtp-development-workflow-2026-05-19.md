---
title: "Modernizing Haskell GTP development workflow"
date: 2026-05-19
last_updated: 2026-05-21
category: developer-experience
module: "Kurt development workflow"
problem_type: developer_experience
component: development_workflow
severity: medium
applies_when:
  - "Reviving an older Haskell Cabal or Stack project on a modern toolchain"
  - "Connecting a stdin/stdout GTP engine to remote GUI clients over SSH"
  - "Adding agent-facing project guidance and smoke/unit tests to a legacy repository"
tags: [haskell, cabal, devenv, gtp, sabaki, ssh, smoke-test, tasty]
---

# Modernizing Haskell GTP development workflow

## Context

Kurt is an older Haskell Computer Go engine that speaks Go Text Protocol (GTP) over stdin/stdout. The repository had enough historical metadata to understand the project, but a fresh agent or Mac GUI client workflow needed several missing pieces: agent instructions, reproducible development dependencies, a modern build path, GTP-safe wrappers, clean `quit` behavior, black-box protocol regression tests, and restored internal Haskell unit/property tests.

The historical Stack setup pinned `lts-2.14` and GHC 7.8.4. Current Stack releases can install that compiler, but then fail because they no longer support the Cabal library bundled with GHC 7.8.4. Modern Cabal with the current Nixpkgs GHC used in this revival could build the project after small compatibility fixes.

A session-history search found no prior relevant attempts for this problem. A later session-history search for the test-restoration follow-up also found no relevant prior sessions.

## Guidance

When reviving a legacy Haskell GTP engine for current agent and GUI workflows, solve the workflow as a thin compatibility layer rather than rewriting the engine.

1. Document the repository for agents.
   - Add `AGENTS.md` with project structure, build commands, smoke/unit test commands, caveats, and GTP expectations.
   - Include specific notes about stdout cleanliness because GTP clients parse stdout as protocol data.
   - Surface `docs/solutions/` so future agents can find documented fixes before re-solving the same workflow problems.

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

4. Put development dependencies and commands in `devenv.nix`.
   - Include `ghc`, `cabal-install`, and `python3` to document the modern build/test path and the Python GTP regression harness. Keep Stack out of active tooling unless it is deliberately reintroduced with a current resolver.
   - Add scripts for the verified build, Haskell test suite, and GTP regression suite.

   ```nix
   { pkgs, ... }:

   {
     packages = [
       pkgs.ghc
       pkgs.cabal-install
       pkgs.python3
     ];

     scripts.build.exec = "cabal v2-build";
     scripts.test.exec = "cabal v2-test";
     scripts.smoke.exec = "scripts/gtp-regression";
   }
   ```

5. Use wrapper scripts for GUI and remote clients.
   - GTP is stdin/stdout, so `ssh host /path/to/wrapper` is already a protocol bridge.
   - Prefer a local wrapper for Sabaki instead of complex inline SSH args when GUI argument parsing is unreliable.
   - The engine-side wrapper (`scripts/kurt-gtp`) centralizes binary lookup. If it must build first, redirect build output away from stdout so GTP clients do not see non-protocol text.
   - Do not assume interactive SSH has the same PATH as the agent shell; handle missing `cabal` by using an existing build artifact or a Nix fallback.

   ```sh
   # Shortened from scripts/kurt-gtp; keep the real script as source of truth.
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

   exec nix shell nixpkgs#ghc nixpkgs#cabal-install \
     -c bash -lc 'cabal v2-build >/dev/null && exec "$(cabal list-bin kurt)" +RTS -N1'
   ```

6. Make `quit` a clean GTP exit.
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

7. Protect the GTP client contract with black-box regression tests.
   - Add a local executable harness that drives Kurt as a stdin/stdout GTP engine.
   - Validate protocol basics, cheap gameplay, generated-move response shape, wrapper startup, strict stdout cleanliness, and clean `quit`.
   - Keep stderr available for diagnostics, but fail on non-GTP stdout because GUI clients parse stdout as the protocol stream.

   ```sh
   scripts/gtp-regression
   devenv shell smoke
   ```

   This should test both direct engine startup and the client-style `scripts/kurt-gtp` wrapper path. Keep live SSH and Sabaki automation out of the required local suite.

8. Restore internal Haskell tests through Cabal separately from GTP tests.
   - Replace stale test files that import removed modules instead of recreating compatibility modules only for historical shape.
   - Preserve the old property intent in current terms: coordinate/board invariants are better than dead `vertexToInt`/`intToVertex` imports.
   - Use a Cabal `test-suite` and a maintained test stack such as Tasty, HUnit, and QuickCheck.
   - In an executable-only package, it is acceptable for a first slice to give the test suite `hs-source-dirs: test, src` and duplicate needed source-module metadata. Defer a library refactor unless duplicate source wiring becomes unmaintainable.

   ```cabal
   test-suite kurt-test
     type:           exitcode-stdio-1.0
     main-is:        Test.hs
     hs-source-dirs: test, src
     build-depends: base >= 3 && < 5
                  , tasty
                  , tasty-hunit
                  , tasty-quickcheck
                  -- plus source-module dependencies
   ```

   The restored suite can cover stable exported behavior without touching engine randomness:
   - `Data.Goban.Types`: GTP coordinate round trips inside valid domains, skipped `I`, board/border vertex invariants, move/color helpers.
   - `Data.Goban.GameState`: new-board fields, pass/resign history, single-stone placement, fresh `nextMoves`, empty-board score.
   - `Data.Goban.Incremental`: chain/liberty smoke checks and same-color merge through exported chain fields.
   - `Data.Goban.ZobristHash`: valid-domain XOR toggle and order-independence properties.

   ```sh
   cabal v2-test
   devenv shell test
   ```

## Why This Matters

Legacy engine projects often fail at the workflow boundary before the domain logic can be tested. Modernizing build metadata, documenting the environment, adding GTP-safe wrappers, and restoring tests makes the engine usable from agents, CLI checks, and Mac GUI clients without changing search or rules logic.

Clean stdout is especially important for GTP. Build logs, shell banners, or crash traces on stdout can corrupt the protocol stream. Wrapper scripts, clean `quit` handling, and strict black-box regression tests keep GUI clients such as Sabaki from seeing confusing non-GTP output.

Separate test layers prevent false confidence. Haskell unit/property tests catch internal invariant regressions quickly, while black-box GTP tests protect the client-visible protocol contract.

## When to Apply

- When a Haskell project has historical Stack or Cabal config but no current development shell.
- When modern Cabal reports missing home modules for an old executable-only package.
- When an engine protocol uses stdin/stdout and needs to be driven remotely from a desktop GUI.
- When a protocol `quit` command exits by exception rather than returning a normal protocol response.
- When stale tests import removed modules but still capture useful property intent.
- When you need both internal invariant tests and black-box protocol tests without conflating the two.

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

### Sabaki wrapper configuration

After pulling the repo on a Mac, configure Sabaki to run a local wrapper with empty args:

```json
{
  "name": "lefant-kurt.exe.xyz",
  "path": "/path/to/kurt/scripts/kurt-remote-gtp",
  "args": ""
}
```

### Local verification ladder

```sh
devenv shell build
devenv shell test
devenv shell smoke
```

Use `build` for compile health, `test` for internal Haskell invariants, and `smoke` for client-facing GTP behavior.

## Related

- Related files: `AGENTS.md`, `devenv.nix`, `kurt.cabal`, `scripts/kurt-gtp`, `scripts/kurt-remote-gtp`, `scripts/gtp-regression`, `test/Test.hs`, `src/Kurt/MainLoop.hs`, `src/Kurt/GoEngine.hs`, `src/Data/Goban/Types.hs`, `src/Data/Tree/UCT/GameTree.hs`.
- Related requirements and plans: `docs/brainstorms/gtp-regression-test-suite-requirements.md`, `docs/plans/2026-05-19-001-feature-gtp-regression-test-suite-plan.md`, `docs/brainstorms/haskell-unit-test-restoration-requirements.md`, `docs/plans/2026-05-20-001-feat-haskell-unit-test-restoration-plan.md`.
- This doc was updated instead of creating a duplicate because the new work had high overlap with the existing Haskell GTP workflow modernization guidance.
