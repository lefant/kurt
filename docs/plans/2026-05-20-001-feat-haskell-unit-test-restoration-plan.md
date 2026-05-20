---
title: Restore Haskell Unit Tests
type: feat
status: completed
date: 2026-05-20
origin: docs/brainstorms/haskell-unit-test-restoration-requirements.md
---

# Restore Haskell Unit Tests

## Summary

Add a Cabal-backed Haskell test suite that replaces the stale unwired test artifact with current QuickCheck-style and deterministic invariant coverage. The plan keeps the first slice focused on exported board, coordinate, game-state, chain, and hash behavior without refactoring the engine or recreating removed modules.

---

## Problem Frame

The origin requirements define the WHAT: restore internal Haskell unit/property testing so `cabal v2-test` becomes a useful verification command. Current repo research confirms the only test file imports absent modules and Cabal has no test-suite stanza.

---

## Requirements

- R1. The project must expose a Haskell test suite through Cabal so `cabal v2-test` builds and runs the restored tests.
- R2. The test suite must be fast enough for normal local development and non-interactive agent verification.
- R3. The restored suite must preserve the intent of the stale QuickCheck properties rather than strictly preserving obsolete imports, module names, or file shape.
- R4. The first restored slice must include property-style coverage for stable low-level board behavior that currently replaces the old vertex/index round-trip intent.
- R5. The first restored slice must include several stable board or game-state invariants, not only a single smoke test.
- R6. Tests must be deterministic enough that failures point to a real behavior or compatibility problem, not normal engine randomness.
- R7. Documentation or agent guidance must identify `cabal v2-test` as the Haskell unit/property test command once restored.
- R8. The restored Haskell tests must complement the existing GTP regression suite rather than replace it.

**Origin actors:** A1 local developer, A2 agent or CI-like runner, A3 future planner/implementer
**Origin flows:** F1 Cabal test restoration, F2 property-intent preservation
**Origin acceptance examples:** AE1 Cabal test command, AE2 stale property intent replacement, AE3 deterministic invariants, AE4 internal-vs-GTP test layering

---

## Scope Boundaries

- Do not recreate removed modules solely for historical compatibility.
- Do not preserve the old test file structure if current modules make a cleaner suite possible.
- Do not test engine strength, move quality, playout quality, or search convergence.
- Do not replace `scripts/gtp-regression`; keep it as the client-contract layer.
- Do not add live SSH, Sabaki, or remote integration testing.
- Avoid broad board, rules, or search refactors unless implementation finds a small change required for testability.

### Deferred to Follow-Up Work

- Deeper rule coverage: ko edge cases, captures, suicide detection, territory/coloring, scoring subtleties, and full legal-move rule behavior should be added after the first restored suite is stable.
- Search/UCT correctness: deterministic tests around tree behavior can follow separately if needed.
- CI integration: adding a hosted workflow is separate from making `cabal v2-test` work locally.

---

## Context & Research

### Relevant Code and Patterns

- `test/Test-Data.Goban.STVector.hs` is stale: it imports `Data.Goban.Goban` and `Data.Goban.STVector`, neither of which exists in this checkout.
- `kurt.cabal` currently defines only the `kurt` executable. There is no `library` stanza and no `test-suite` stanza.
- `src/Data/Goban/Types.hs` exports stable pure functions and constructors for coordinates, moves, colors, board vertices, border vertices, adjacency, and GTP coordinate rendering.
- `src/Data/Goban/GameState.hs` exports `GameState(..)` and `GameStateStuff(..)`, so tests can inspect stable fields such as `boardsize`, `freeVerticesSet`, `moveHistory`, stone counts, `koBlocked`, and `zHash`.
- `src/Data/Goban/Incremental.hs` exports enough chain behavior for black-box-ish internal tests: `newGobanMap`, `newChainMap`, `addStone`, `vertexChain`, `allStones`, `isSuicide`, `Chain(..)`, and `colorTerritories`.
- `src/Data/Goban/ZobristHash.hs` exposes hash update behavior, but supported key space is partial and should be tested only inside valid vertices/states.
- `AGENTS.md` already distinguishes the GTP regression suite from future Haskell unit/property restoration.

### Institutional Learnings

- `docs/solutions/developer-experience/modernizing-haskell-gtp-development-workflow-2026-05-19.md` emphasizes small modernization steps and preserving the GTP stdout contract. The Haskell test restoration should not disturb the wrapper/GTP path.

### External References

- No external research is needed for this plan. Local Cabal conventions and current module exports provide enough grounding for the first restored slice.

---

## Key Technical Decisions

- Use a Cabal `test-suite` before a library refactor: Adding a test suite with access to `src` is the smallest change that satisfies `cabal v2-test` without restructuring the package.
- Replace the stale test artifact rather than patching missing imports: Current modules provide better stable seams than recreated compatibility modules.
- Use Tasty for the restored suite: Prefer `tasty`, `tasty-hunit`, and `tasty-quickcheck` so the suite keeps property coverage without reviving the obsolete `test-framework` dependency path.
- Start with exported invariant coverage: Tests should rely on public module exports, constructors, and record fields, not private chain IDs or unexported helpers.
- Add a devenv convenience alias only as a wrapper around Cabal: `cabal v2-test` remains the authoritative interface, while devenv can make it discoverable alongside `build` and `smoke`.

---

## Open Questions

### Resolved During Planning

- Test interface: `cabal v2-test` is the main required interface.
- Test framework: Use Tasty with HUnit and QuickCheck providers for a maintained mixed example/property suite.
- Old test shape: Preserve the old property intent, not obsolete imports or file layout.
- First slice breadth: Build a foundation suite with several stable invariants instead of one tiny smoke test.
- Closest replacement for old vertex/index round trips: Use current coordinate and board-structure invariants from `Data.Goban.Types`, then add game-state and incremental-board invariants for broader foundation coverage.

### Deferred to Implementation

- Exact test file layout: Choose the simplest layout that keeps the suite readable and easy to extend.
- Exact invariant count: Implement the stable scenarios below, but keep the final number right-sized if one proves brittle or overly coupled during implementation.

---

## Implementation Units

- U1. **Wire a Cabal Haskell test suite**

**Goal:** Make `cabal v2-test` build and execute a current Haskell test entry point.

**Requirements:** R1, R2, R7, AE1

**Dependencies:** None

**Files:**
- Modify: `kurt.cabal`
- Modify: `devenv.nix`
- Create: `test/Test.hs`
- Test: `test/Test.hs`

**Approach:**
- Add a Cabal test-suite stanza named `kurt-test` using Cabal's `exitcode-stdio-1.0` interface and `test/Test.hs` as the entry point.
- Use `hs-source-dirs` that include both `test` and `src` so tests can import current source modules without first restructuring the package into a library.
- Add explicit test dependencies for Tasty-based unit/property tests: `tasty`, `tasty-hunit`, and `tasty-quickcheck`, alongside the source-module dependencies needed by imported modules.
- Duplicate required source module metadata in the test suite (`other-modules`, `default-language`, relevant `default-extensions`) rather than refactoring to a library in this slice, unless duplicate source wiring proves impossible.
- Keep the existing executable stanza intact.
- Add a devenv `test` convenience command that delegates to `cabal v2-test`, while documenting Cabal as the authoritative interface.

**Execution note:** Start with the smallest failing `cabal v2-test` path, then add real invariants in later units.

**Patterns to follow:**
- Existing executable dependency style in `kurt.cabal`.
- Existing `devenv.nix` `build` and `smoke` script definitions.

**Test scenarios:**
- Integration: `cabal v2-test` discovers and runs the new Haskell test suite.
- Error path: a deliberately failing assertion would surface through Cabal's normal test output with the failing test name.
- Integration: the existing `kurt` executable build remains available after adding the test-suite stanza.

**Verification:**
- `cabal v2-test` runs the new suite successfully.
- The new test command does not replace or interfere with `scripts/gtp-regression`.

---

- U2. **Restore low-level board and coordinate properties**

**Goal:** Replace the stale vertex/index round-trip intent with current stable low-level board and coordinate invariants.

**Requirements:** R3, R4, R6, AE2

**Dependencies:** U1

**Files:**
- Modify: `test/Test.hs`
- Modify/Delete: `test/Test-Data.Goban.STVector.hs`
- Test: `test/Test.hs`

**Approach:**
- Remove or replace the stale test file so it no longer imports absent modules.
- Add property-style tests around current exported coordinate and board functions.
- Keep partial functions inside their valid domains so properties describe stable behavior rather than crash behavior.
- Avoid asserting implementation details that are not part of the useful board contract.

**Execution note:** Characterization-first: preserve the old test's round-trip spirit by first encoding current coordinate round-trip behavior before broader invariants.

**Patterns to follow:**
- `src/Data/Goban/Types.hs` exports: `allVertices`, `borderVertices`, `adjacentVertices`, `diagonalVertices`, `xToLetter`, `letterToX`, `gtpShowVertex`, `gtpShowMove`, `otherColor`, `moveColor`, and `isStoneMove`.

**Test scenarios:**
- Happy path: valid x coordinates round-trip through `xToLetter` and `letterToX`.
- Edge case: GTP coordinate rendering skips the `I` column for valid coordinates around the boundary.
- Happy path: `allVertices` for a positive board has exactly `n * n` unique in-board vertices.
- Edge case: `borderVertices` does not overlap with `allVertices` for positive board sizes.
- Happy path: adjacent and diagonal vertex helpers return stable neighbor sets for representative points.
- Happy path: move/color helpers report expected colors and distinguish stone moves from pass/resign.

**Verification:**
- The stale missing-module imports are gone from runnable tests.
- Current low-level property tests pass repeatedly without randomness.

---

- U3. **Add deterministic game-state invariants**

**Goal:** Cover stable `GameState` behavior for new boards, pass/resign moves, simple stone placement, scoring basics, and legal next-move shape.

**Requirements:** R5, R6, R8, AE3, AE4

**Dependencies:** U1, U2

**Files:**
- Modify: `test/Test.hs`
- Test: `test/Test.hs`

**Approach:**
- Test only deterministic `GameState` transitions and exported fields.
- Use small boards and explicit moves; avoid search, playouts, or random engine behavior.
- Compare stable observable fields rather than relying on hidden internals.

**Patterns to follow:**
- `src/Data/Goban/GameState.hs` exports: `newGameState`, `updateGameState`, `nextMoves`, `scoreGameState`, `freeVertices`, `thisMoveColor`, `nextMoveColor`, and record fields through `GameState(..)` / `GameStateStuff(..)`.
- `src/Data/Goban/Types.hs` move constructors.

**Test scenarios:**
- Happy path: a new game state has the requested board size, komi, no stones, no ko block, empty history, zero hash, and `n * n` free vertices.
- Happy path: initial `nextMoveColor` is black.
- Happy path: pass and resign append to move history without changing stones or free vertices.
- Happy path: placing one black stone updates stone count, move history, free-vertex set, and chain-visible stone membership.
- Edge case: `nextMoves` on a fresh board includes pass and all in-board stone moves for the requested color.
- Happy path: empty-board score equals negative komi for simple komi values.

**Verification:**
- The suite covers stable game-state behavior without invoking GTP or engine search.
- Repeated `cabal v2-test` runs produce deterministic results.

---

- U4. **Add incremental board and hash invariants**

**Goal:** Extend foundation coverage to chain/liberty smoke invariants and safe Zobrist hash algebra.

**Requirements:** R5, R6, AE3

**Dependencies:** U1, U2

**Files:**
- Modify: `test/Test.hs`
- Test: `test/Test.hs`

**Approach:**
- Use small deterministic board positions built through exported incremental-board functions.
- Assert chain behavior through exported `Chain` fields and vertex queries, not internal chain IDs.
- Test Zobrist hash algebra only for supported vertices/states.

**Patterns to follow:**
- `src/Data/Goban/Incremental.hs` exports: `newGobanMap`, `newChainMap`, `addStone`, `vertexChain`, `allStones`, and `Chain(..)`.
- `src/Data/Goban/ZobristHash.hs` exports: `updateHash` and valid `VertexState` values from `Data.Goban.Types`.

**Test scenarios:**
- Happy path: adding a center stone produces one chain with four liberties.
- Edge case: adding a corner stone produces a chain with two liberties.
- Happy path: adjacent same-color stones merge into one observable chain vertex set.
- Happy path: applying the same valid Zobrist update twice returns the original hash.
- Happy path: applying two distinct valid Zobrist updates is order-independent.

**Verification:**
- Chain and hash tests rely only on exported behavior.
- The tests avoid unsupported hash keys and private chain IDs.

---

- U5. **Document restored Haskell test workflow**

**Goal:** Make the restored Haskell test command discoverable without confusing it with GTP regression coverage.

**Requirements:** R7, R8, AE1, AE4

**Dependencies:** U1, U2, U3, U4

**Files:**
- Modify: `AGENTS.md`
- Modify: `devenv.nix`
- Test: `test/Test.hs`

**Approach:**
- Update developer/agent guidance to name `cabal v2-test` as the Haskell unit/property test command.
- Keep `scripts/gtp-regression` documented as the GTP/client-contract suite.
- If a devenv `test` alias is added in U1, document it as a convenience wrapper around Cabal rather than a separate test layer.

**Patterns to follow:**
- Existing `AGENTS.md` sections for build and smoke testing.
- Existing `devenv.nix` script naming style.

**Test scenarios:**
- Documentation expectation: guidance clearly separates internal Haskell tests from GTP regression tests.
- Integration: documented Haskell test command runs the same suite as Cabal.

**Verification:**
- A developer can identify both verification layers: `cabal v2-test` for Haskell invariants and `scripts/gtp-regression` / `devenv shell smoke` for GTP behavior.

---

## System-Wide Impact

- **Interaction graph:** Test wiring touches Cabal package metadata and source-module import paths, but should not alter runtime engine behavior.
- **Error propagation:** Test failures should surface through Cabal/Tasty output, not GTP stdout.
- **State lifecycle risks:** Tests may expose existing behavior in board or game-state logic; implementation should avoid changing runtime behavior unless a test reveals a clear bug and the change is intentionally scoped.
- **API surface parity:** Cabal build, `devenv` helper scripts, and `AGENTS.md` should agree on test commands.
- **Integration coverage:** Cabal tests cover internal invariants; `scripts/gtp-regression` remains the integration-like client contract check.
- **Unchanged invariants:** GTP wrapper stdout cleanliness and `quit` behavior remain governed by the existing regression suite, not by this Haskell test slice.

---

## Risks & Dependencies

| Risk | Mitigation |
|------|------------|
| Adding test dependencies breaks modern Cabal resolution | Use the explicit Tasty dependency set from U1 and verify with `cabal v2-test` in the current devenv. |
| Tests couple to private implementation details | Use only exported functions, constructors, and record fields; avoid chain IDs and unexported helpers. |
| Properties include partial-function crash domains | Restrict generated values to valid coordinate/hash domains. |
| A test reveals existing behavior that looks wrong but is outside this slice | Record it as follow-up unless it blocks the restored suite or invalidates a required invariant. |
| Cabal test wiring duplicates source-module lists | Accept small duplication for this slice; defer library restructuring unless it becomes necessary. |

---

## Documentation / Operational Notes

- Update `AGENTS.md` so future agents know when to run `cabal v2-test` versus `scripts/gtp-regression`.
- No production/runtime monitoring is required; this is a local developer verification improvement.
- If test dependency resolution changes the generated Cabal artifacts or lock-like files, document the intended reproducible path through devenv.

---

## Sources & References

- **Origin document:** [docs/brainstorms/haskell-unit-test-restoration-requirements.md](../brainstorms/haskell-unit-test-restoration-requirements.md)
- Related plan: [docs/plans/2026-05-19-001-feature-gtp-regression-test-suite-plan.md](2026-05-19-001-feature-gtp-regression-test-suite-plan.md)
- Current stale test: `test/Test-Data.Goban.STVector.hs`
- Cabal metadata: `kurt.cabal`
- Core types: `src/Data/Goban/Types.hs`
- Game state: `src/Data/Goban/GameState.hs`
- Incremental board: `src/Data/Goban/Incremental.hs`
- Zobrist hashing: `src/Data/Goban/ZobristHash.hs`
- Agent guidance: `AGENTS.md`
