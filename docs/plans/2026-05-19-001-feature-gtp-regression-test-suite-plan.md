---
date: 2026-05-19
status: completed
type: feature
topic: gtp-regression-test-suite
origin: docs/brainstorms/gtp-regression-test-suite-requirements.md
---

# GTP Regression Test Suite Plan

## Summary

Implement a small local black-box GTP regression suite that runs Kurt as a stdin/stdout engine and validates the client-facing contract. The suite will cover protocol basics, basic gameplay, wrapper startup, clean quit behavior, and strict stdout cleanliness, then wire the existing devenv smoke command to the suite.

---

## Problem Frame

Kurt now builds with a modern Cabal/GHC path and has wrapper scripts for GUI/CLI GTP clients, but the protection around that workflow is still mostly manual. A future change could break clean `quit`, leak build output to stdout, or return malformed client-visible responses without being caught until Sabaki or another GTP client fails.

The existing Haskell test under `test/` is stale and unwired. Restoring internal unit/property coverage is valuable, but the immediate first slice should protect the external GTP protocol contract without requiring a broader Haskell test-framework migration.

---

## Requirements

- R1. Verify protocol basics: `name`, `protocol_version`, `known_command`, `list_commands`, and `quit`.
- R2. Verify a basic gameplay flow with board setup, komi setup, clear board, legal play, generated move, and clean quit.
- R3. Validate generated moves at protocol level as an in-board coordinate for the configured board size, `pass`, or `resign`.
- R4. Verify client-style wrapper startup, not only direct executable invocation.
- R5. Verify `quit` returns an empty success response and exits with status 0.
- R6. Fail on non-GTP text in stdout during protocol interaction.
- R7. Tolerate stderr diagnostics for this slice while keeping stdout assertions strict.
- R8. Make the existing devenv smoke command run or delegate to the new local regression suite.
- R9. Produce actionable failure output that identifies the failing scenario or assertion.
- R10. Run locally without live SSH, Sabaki automation, or network availability.

**Origin actors:** A1 local developer, A2 agent or CI-like runner, A3 GTP client emulator
**Origin flows:** F1 local regression run, F2 gameplay smoke sequence
**Origin acceptance examples:** AE1 protocol basics and clean quit, AE2 gameplay response shape, AE3 wrapper stdout cleanliness, AE4 local documented smoke command

---

## Scope Boundaries

- Live SSH testing against `lefant-kurt.exe.xyz` is excluded from this plan.
- Sabaki GUI automation is excluded from this plan.
- Engine strength, move quality, or game outcome evaluation is excluded from this plan.
- Full GTP spec compliance is excluded from this plan.
- Restoring the stale Haskell QuickCheck/unit test file is deferred to follow-up work.
- GTP handler refactoring is excluded unless implementation reveals it is required for black-box testability.

### Deferred to Follow-Up Work

- Haskell unit/property test restoration: repair or replace `test/Test-Data.Goban.STVector.hs` and wire it into Cabal in a separate testing slice.
- Remote integration testing: add opt-in SSH/Sabaki-adjacent coverage after the local suite is stable.

---

## Context & Research

### Relevant Code and Patterns

- `scripts/kurt-gtp` is the client-style local wrapper and must remain stdout-clean for GTP clients.
- `scripts/kurt-remote-gtp` is out of required test scope because it requires live SSH.
- `devenv.nix` already contains `build` and `smoke` scripts; `smoke` should delegate to the new regression suite.
- `src/Kurt/MainLoop.hs` owns GTP command dispatch and clean quit behavior.
- `src/Data/Goban/Types.hs` defines GTP coordinate rendering and the skipped `I` column convention.
- `test/Test-Data.Goban.STVector.hs` is stale, imports missing modules, and is not a pattern to follow for this slice.

### Institutional Learnings

- `docs/solutions/developer-experience/modernizing-haskell-gtp-development-workflow-2026-05-19.md` emphasizes that GTP stdout must stay protocol-clean and that wrappers should keep build output away from stdout.

### External References

- No external research is required for this first slice. Local GTP behavior and existing wrapper scripts provide enough grounding.

---

## Key Technical Decisions

- Use a black-box script harness: It directly exercises the client contract and avoids coupling the first slice to a Haskell test-framework migration.
- Keep stdout validation strict: GTP clients parse stdout as protocol data, so stdout pollution should fail even if stderr remains noisy.
- Exercise both direct and wrapper startup: Direct execution isolates engine behavior, while wrapper execution protects the GUI/client path.
- Keep remote testing out of the required path: The first suite should be deterministic and network-independent.
- Validate generated moves by response shape and board bounds, not engine quality: The suite protects protocol correctness, not playing strength.

---

## Open Questions

### Resolved During Planning

- Coordinate validation rule: For the first slice, validate uppercase GTP coordinates against the configured board size and skip the `I` column; also accept `pass` and `resign`.
- Wrapper scope: Exercise `scripts/kurt-gtp` locally; do not exercise `scripts/kurt-remote-gtp` in the required suite.
- Harness style: Use a lightweight executable script with scenario names and assertion output rather than introducing a Haskell test framework in this slice.

### Deferred to Implementation

- Exact parser shape for GTP response blocks: The implementation can choose the simplest robust parser that distinguishes stdout protocol responses from non-protocol text.
- Exact temporary file handling: The implementation can choose temp file or in-memory capture as long as failures are actionable and cleanup is reliable.

---

## Output Structure

    scripts/
      gtp-regression

---

## Implementation Units

- U1. **Create black-box GTP regression harness**

**Goal:** Add a local executable harness that can run a command as a GTP engine, send scripted command sequences, capture stdout/stderr/status, and report scenario-level assertion failures.

**Requirements:** R1, R2, R5, R6, R7, R9, R10

**Dependencies:** None

**Files:**
- Create: `scripts/gtp-regression`
- Modify: none
- Test: `scripts/gtp-regression`

**Approach:**
- Build the harness as a small repo-local script so it runs in the existing devenv without new Haskell test dependencies.
- Model tests as named scenarios that provide input commands and assertions over stdout, stderr, and exit status.
- Treat stdout as a sequence of GTP response blocks and fail if any non-empty stdout line does not look like a GTP success or failure response line.
- Keep stderr available in failure output, but do not fail solely because existing trace diagnostics appear on stderr.

**Execution note:** Test-first by running the harness against a deliberately small protocol-basics scenario before adding the gameplay and wrapper scenarios.

**Patterns to follow:**
- `devenv.nix` smoke command for current GTP command sequence.
- `scripts/kurt-gtp` for wrapper invocation and stdout-clean build behavior.

**Test scenarios:**
- Happy path: protocol-basics scenario sends `name`, `protocol_version`, and `quit` to a direct engine command -> stdout contains `kurt`, `2`, and an empty success response; exit status is 0.
- Error path: assertion failure reports the scenario name and the missing or malformed expected response.
- Edge case: stderr contains trace output -> scenario can still pass when stdout and exit status are correct.
- Integration: harness runs without network access or SSH configuration.

**Verification:**
- `scripts/gtp-regression` runs locally and reports pass/fail with scenario names.

---

- U2. **Add required GTP scenarios**

**Goal:** Encode the required local coverage: protocol basics, gameplay flow, generated-move validation, clean quit, and stdout cleanliness.

**Requirements:** R1, R2, R3, R5, R6, R7, R9, R10

**Dependencies:** U1

**Files:**
- Create: none
- Modify: `scripts/gtp-regression`
- Test: `scripts/gtp-regression`

**Approach:**
- Add a protocol-basics scenario covering `name`, `protocol_version`, `known_command`, `list_commands`, and `quit`.
- Add a gameplay scenario using a small board and cheap engine settings so `genmove` remains fast.
- Validate generated moves as `pass`, `resign`, or a coordinate within the configured board with GTP's skipped `I` column.
- Assert clean quit status for every scenario that terminates the engine with `quit`.

**Execution note:** Characterization-first: capture current passing responses for the existing smoke flow, then tighten assertions to the requirements above.

**Patterns to follow:**
- `src/Data/Goban/Types.hs` coordinate convention for GTP letters.
- Current `devenv.nix` smoke sequence for cheap gameplay setup.

**Test scenarios:**
- Happy path: `known_command name` returns true and `known_command not-a-command` returns false.
- Happy path: `list_commands` output includes core commands used by the suite.
- Happy path: gameplay scenario returns a coordinate within the small board, `pass`, or `resign` for `genmove`.
- Edge case: coordinate validation rejects `I` column and coordinates outside the configured board.
- Error path: stdout line that is not a GTP response line fails the scenario.
- Integration: all required scenarios pass against direct local engine execution.

**Verification:**
- Direct local execution of `scripts/gtp-regression` covers every origin acceptance example except wrapper startup, which is handled by U3.

---

- U3. **Cover client-style wrapper startup**

**Goal:** Ensure the local wrapper used by GUI/client workflows can run the same protocol-basics scenario without polluting stdout.

**Requirements:** R4, R5, R6, R7, R9, R10

**Dependencies:** U1, U2

**Files:**
- Create: none
- Modify: `scripts/gtp-regression`
- Test: `scripts/gtp-regression`

**Approach:**
- Add a wrapper scenario that invokes `scripts/kurt-gtp` as the engine command.
- Reuse the protocol-basics assertions for wrapper execution.
- Keep `scripts/kurt-remote-gtp` out of the required path because it depends on SSH and network availability.

**Execution note:** Characterization-first: verify current wrapper behavior before adding stricter stdout assertions.

**Patterns to follow:**
- `scripts/kurt-gtp` wrapper behavior.
- `docs/solutions/developer-experience/modernizing-haskell-gtp-development-workflow-2026-05-19.md` guidance on GTP-safe wrappers.

**Test scenarios:**
- Happy path: wrapper scenario returns correct protocol-basics responses and exit status 0.
- Edge case: wrapper must not emit build output on stdout before the first GTP response.
- Error path: missing runnable engine path or wrapper failure produces an actionable scenario failure.
- Integration: wrapper scenario passes without network access.

**Verification:**
- `scripts/gtp-regression` includes and passes a wrapper startup scenario.

---

- U4. **Wire regression suite into developer workflow and docs**

**Goal:** Make the new suite the documented smoke path for developers and agents.

**Requirements:** R8, R9, R10

**Dependencies:** U1, U2, U3

**Files:**
- Create: none
- Modify: `devenv.nix`
- Modify: `AGENTS.md`
- Test: `scripts/gtp-regression`

**Approach:**
- Update the devenv smoke script to delegate to the regression harness rather than duplicating inline GTP assertions.
- Update agent guidance to name the regression suite as the local smoke/regression command.
- Keep the existing build command unchanged.

**Execution note:** Update docs after the suite passes so documented commands match verified behavior.

**Patterns to follow:**
- Existing `devenv.nix` script definitions.
- Existing `AGENTS.md` smoke testing section.

**Test scenarios:**
- Happy path: `devenv shell smoke` runs the regression suite and succeeds locally.
- Error path: a failing regression scenario causes the smoke command to fail nonzero.
- Integration: documented smoke command and direct harness command cover the same required scenarios.

**Verification:**
- `devenv shell smoke` delegates to the new suite and passes.
- `AGENTS.md` points agents to the new suite.

---

## System-Wide Impact

- **Interaction graph:** The suite launches Kurt as an external process through direct and wrapper entry points; it does not call Haskell modules directly.
- **Error propagation:** Harness failures should propagate as nonzero process exits with scenario names and captured output snippets.
- **State lifecycle risks:** Each scenario should use a fresh engine process so board state does not leak between scenarios.
- **API surface parity:** Local wrapper coverage protects the GUI-style startup path; remote wrapper parity is explicitly deferred.
- **Integration coverage:** Black-box process tests cover build/run/wrapper/protocol integration that internal unit tests would not prove.
- **Unchanged invariants:** The GTP engine interface remains stdin/stdout; stderr trace behavior is tolerated in this slice.

---

## Risks & Dependencies

| Risk | Mitigation |
|------|------------|
| Trace output or diagnostics make assertions flaky | Assert strictly on stdout only; include stderr in failure reports for debugging. |
| `genmove` is slow or nondeterministic | Use small board and cheap engine configuration; validate response shape rather than exact move. |
| Wrapper builds during test and slows the suite | Accept initial build cost but keep build output off stdout; subsequent runs should use existing artifacts. |
| Script harness becomes too ad hoc | Keep scenarios small and named; defer richer framework decisions to follow-up if the suite grows. |
| Coordinate validation diverges from engine convention | Base validation on GTP skipped-`I` letters and configured board size. |

---

## Documentation / Operational Notes

- Update `AGENTS.md` so future agents know the regression suite is the authoritative local smoke path.
- Keep remote Sabaki/SSH instructions separate from required local regression coverage.
- If later CI is added, the local regression suite should be the first candidate command to run there.

---

## Sources & References

- **Origin document:** [docs/brainstorms/gtp-regression-test-suite-requirements.md](../brainstorms/gtp-regression-test-suite-requirements.md)
- **Institutional learning:** [docs/solutions/developer-experience/modernizing-haskell-gtp-development-workflow-2026-05-19.md](../solutions/developer-experience/modernizing-haskell-gtp-development-workflow-2026-05-19.md)
- Related code: `scripts/kurt-gtp`
- Related code: `scripts/kurt-remote-gtp`
- Related code: `devenv.nix`
- Related code: `AGENTS.md`
- Related code: `src/Kurt/MainLoop.hs`
- Related code: `src/Data/Goban/Types.hs`
