---
date: 2026-05-20
topic: haskell-unit-test-restoration
---

# Haskell Unit Test Restoration

## Summary

Restore Kurt's Haskell unit/property testing as a Cabal-backed foundation slice. The restored suite should preserve the stale QuickCheck test's intent while shifting to currently available modules and adding several stable board/game invariants that complement the black-box GTP regression suite.

---

## Problem Frame

Kurt now has a modern build path and a local black-box GTP regression suite, but internal Haskell behavior still has no working test coverage. The only file under `test/` imports modules that are not present in this checkout and is not wired into Cabal, so `cabal v2-test` cannot currently serve as a meaningful project verification command.

The stale test appears to have protected low-level board coordinate/index round-trip behavior. That exact API no longer exists in the current source tree, but the underlying need remains: small, fast properties should catch regressions in core board and game-state assumptions before they surface as confusing GTP behavior or search bugs.

---

## Actors

- A1. Local developer: runs Haskell tests while changing board, rules, parser-adjacent, or game-state code.
- A2. Agent or CI-like runner: runs `cabal v2-test` non-interactively and needs deterministic pass/fail output.
- A3. Future planner/implementer: uses this requirements document to decide the first restored internal-test slice without re-litigating scope.

---

## Key Flows

- F1. Cabal test restoration
  - **Trigger:** A developer or agent wants to verify internal Haskell behavior.
  - **Actors:** A1, A2
  - **Steps:** Build the project, run `cabal v2-test`, execute the restored property/unit suite, and report failures through the normal Cabal test interface.
  - **Outcome:** `cabal v2-test` is a reliable required verification command for the restored internal-test slice.
  - **Covered by:** R1, R2, R6, R7

- F2. Property-intent preservation
  - **Trigger:** The stale test's old API targets are unavailable during restoration.
  - **Actors:** A3
  - **Steps:** Identify the behavior the stale properties meant to protect, map that behavior to current modules, and encode equivalent or stronger stable invariants.
  - **Outcome:** The restored tests protect meaningful current behavior instead of preserving dead module names for their own sake.
  - **Covered by:** R3, R4, R5

---

## Requirements

**Cabal test interface**
- R1. The project must expose a Haskell test suite through Cabal so `cabal v2-test` builds and runs the restored tests.
- R2. The test suite must be fast enough for normal local development and non-interactive agent verification.
- R3. The restored suite must preserve the intent of the stale QuickCheck properties rather than strictly preserving obsolete imports, module names, or file shape.

**Core behavior coverage**
- R4. The first restored slice must include property-style coverage for stable low-level board behavior that currently replaces the old vertex/index round-trip intent.
- R5. The first restored slice must include several stable board or game-state invariants, not only a single smoke test.
- R6. Tests must be deterministic enough that failures point to a real behavior or compatibility problem, not normal engine randomness.

**Developer workflow**
- R7. Documentation or agent guidance must identify `cabal v2-test` as the Haskell unit/property test command once restored.
- R8. The restored Haskell tests must complement the existing GTP regression suite rather than replace it.

---

## Acceptance Examples

- AE1. **Covers R1, R2, R7.** Given a checkout with the documented development environment, when a developer runs `cabal v2-test`, Cabal builds and executes the Haskell test suite without requiring manual test wiring.
- AE2. **Covers R3, R4.** Given the old vertex/index APIs are not present, when the stale test is restored, the resulting properties still protect equivalent current low-level coordinate or board mapping behavior.
- AE3. **Covers R5, R6.** Given the test suite runs repeatedly, when no code changes are made, the same stable board/game invariants pass consistently without depending on engine playout randomness.
- AE4. **Covers R8.** Given both test layers exist, when a developer verifies a change, Haskell unit/property tests cover internal invariants while `scripts/gtp-regression` continues covering client-visible GTP behavior.

---

## Success Criteria

- `cabal v2-test` becomes a useful, documented verification command for internal Haskell behavior.
- The old stale test is no longer a dead artifact: its intent is either restored in current terms or deliberately replaced by stronger current invariants.
- Downstream planning can choose concrete test dependencies and exact invariants without inventing product scope, non-goals, or success criteria.

---

## Scope Boundaries

- Do not recreate removed modules solely for historical compatibility.
- Do not require preserving the old test file structure if current modules make a cleaner suite possible.
- Do not test engine strength, move quality, playout quality, or search convergence in this slice.
- Do not replace the black-box GTP regression suite; keep it as the client-contract layer.
- Do not add live SSH, Sabaki, or remote integration testing to this slice.
- Avoid broad board/rules/search refactors unless planning finds a small change required for testability.

---

## Key Decisions

- Preserve intent over shape: The old test's imports point at absent modules, so compatibility at the file/module-name level would create carrying cost without preserving more value.
- Use Cabal as the primary interface: `cabal v2-test` is the standard command downstream agents and developers should be able to run.
- Build a foundation slice, not a token smoke test: Several stable internal invariants create a better base for future rule and game-state coverage.
- Keep layers separate: Haskell unit/property tests should catch internal invariant regressions, while `scripts/gtp-regression` protects stdout-clean GTP behavior.

---

## Dependencies / Assumptions

- Current source modules expose enough stable behavior to express useful board and game-state invariants.
- Test dependencies chosen during planning can be made compatible with the modern Cabal/GHC path already used by the project.
- The restored tests can avoid engine randomness and expensive search paths.

---

## Outstanding Questions

### Deferred to Planning

- [Affects R1][Technical] Decide which Haskell test framework and dependency versions fit the current Cabal build with minimal maintenance cost.
- [Affects R4][Needs research] Identify the closest current replacement for the stale `vertexToInt` / `intToVertex` round-trip properties after reading the relevant board modules.
- [Affects R5][Needs research] Select the initial invariant set from current board, coordinate, and game-state behavior.
- [Affects R7][Technical] Decide whether to add a separate convenience command in `devenv.nix` or keep `cabal v2-test` as the only documented unit-test interface.
