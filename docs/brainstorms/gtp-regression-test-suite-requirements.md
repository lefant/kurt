---
date: 2026-05-19
topic: gtp-regression-test-suite
---

# GTP Regression Test Suite

## Summary

Add a small required local GTP regression suite that tests Kurt the way GUI and CLI clients use it: as a stdin/stdout GTP engine. The first slice covers protocol basics, simple gameplay flow, wrapper startup behavior, and clean quit semantics while leaving live SSH/Sabaki automation and deeper Haskell unit/property tests for later.

---

## Problem Frame

Recent work made Kurt build on a modern toolchain, added development-environment documentation, created GTP wrapper scripts, and fixed `quit` so clients no longer see a crash. Those changes were verified manually and through a single smoke command, but the repo still lacks a durable regression suite that captures the client-facing behaviors that matter most.

The current Haskell test file is stale and unwired, so restoring internal unit/property coverage is a separate effort. The immediate risk is simpler: a future change could pollute stdout, break basic GTP responses, regress wrapper startup, or reintroduce bad exit behavior without being caught before a GUI client such as Sabaki fails.

---

## Actors

- A1. Local developer: runs the test suite before committing or while debugging GTP behavior.
- A2. Agent or CI-like runner: runs the same local checks non-interactively and gets clear pass/fail output.
- A3. GTP client emulator: drives Kurt through stdin/stdout and validates client-visible behavior.

---

## Key Flows

- F1. Local regression run
  - **Trigger:** A developer or agent wants to verify client-facing GTP behavior.
  - **Actors:** A1, A2, A3
  - **Steps:** Build or locate a runnable Kurt engine, send deterministic GTP command sequences, collect stdout/stderr/exit status, and assert the expected protocol outcomes.
  - **Outcome:** The run succeeds only when all required local GTP behaviors are intact.
  - **Covered by:** R1, R2, R3, R4, R5

- F2. Gameplay smoke sequence
  - **Trigger:** The suite reaches the basic gameplay coverage portion.
  - **Actors:** A3
  - **Steps:** Configure a small board, set cheap engine limits, clear the board, play a legal move, request a generated move, and quit cleanly.
  - **Outcome:** Kurt returns valid GTP success responses, a legal-looking move/pass/resign response, and exit code 0.
  - **Covered by:** R2, R3, R5

---

## Requirements

**Required local coverage**
- R1. The suite must verify protocol basics: `name`, `protocol_version`, `known_command`, `list_commands`, and `quit`.
- R2. The suite must verify a basic gameplay flow that includes board setup, komi setup, clearing the board, playing a legal move, generating a move, and quitting.
- R3. Generated moves must be validated at the protocol level as a legal-looking board coordinate for the configured board size, `pass`, or `resign`.
- R4. The suite must verify a client-style wrapper startup path, not only direct executable invocation.
- R5. `quit` must return a normal empty success response and exit with status 0.

**Protocol cleanliness**
- R6. Tests must fail if build output, shell banners, crash traces, or other non-GTP text appear on stdout during protocol interaction.
- R7. Diagnostics on stderr may be tolerated, but the suite must keep stdout assertions strict because clients parse stdout as protocol data.

**Developer workflow**
- R8. The existing devenv smoke command must run the new local regression suite or delegate to it.
- R9. Test output must make failures actionable by identifying which command sequence or assertion failed.
- R10. The suite must run locally without live SSH access, Sabaki automation, or network availability.

---

## Acceptance Examples

- AE1. **Covers R1, R5, R6.** Given a freshly built engine, when the suite sends `name`, `protocol_version`, and `quit`, stdout contains only valid GTP response blocks for those commands and the process exits 0.
- AE2. **Covers R2, R3, R5.** Given a cheap small-board gameplay sequence, when the suite requests `genmove`, the response is a coordinate within that board, `pass`, or `resign`, followed by clean `quit` behavior.
- AE3. **Covers R4, R6, R7.** Given the client-style wrapper path, when the suite runs protocol basics through the wrapper, stdout remains GTP-clean even if build or diagnostic output exists elsewhere.
- AE4. **Covers R8, R9, R10.** Given a local development environment with no SSH connectivity, when a developer runs the documented smoke command, the local suite runs and any failure names the failing scenario.

---

## Success Criteria

- A developer can run one documented local command and know whether Kurt's basic GTP client contract still works.
- The suite catches regressions in clean stdout, protocol basics, basic gameplay response shape, wrapper startup, and clean quit behavior.
- Downstream planning does not need to invent the first test slice's scope, non-goals, or pass/fail criteria.

---

## Scope Boundaries

- Live SSH testing against `lefant-kurt.exe.xyz` is out of scope for the first slice.
- Sabaki GUI automation is out of scope for the first slice.
- Engine strength, move quality, or game outcome evaluation is out of scope.
- Full GTP spec compliance is out of scope.
- Restoring the stale Haskell QuickCheck/unit test file is deferred to a later testing slice.
- Refactoring the engine architecture or GTP command handler design is out of scope unless needed to make tests possible.

---

## Key Decisions

- Start with black-box GTP regression tests: This directly protects the client-facing behavior that recently caused manual debugging work.
- Keep remote tests out of the required suite: The first slice should be reliable locally and not depend on SSH, network state, or a specific VM hostname.
- Treat stdout as strict protocol output: GUI clients consume stdout as GTP, so stdout pollution should fail tests even if stderr remains noisy.
- Defer internal Haskell tests: The existing test file is stale and unwired, making it a separate modernization task rather than a prerequisite for the GTP suite.

---

## Dependencies / Assumptions

- Kurt can be built or run through the existing local development environment.
- A wrapper path exists for client-style startup.
- The gameplay sequence can keep move generation cheap enough for a fast local test.
- Stderr traces are acceptable for the first slice as long as stdout stays protocol-clean.

---

## Outstanding Questions

### Deferred to Planning

- [Affects R3][Technical] Decide the exact coordinate validation rule for board sizes and the skipped `I` column.
- [Affects R4][Technical] Decide whether wrapper startup should test the local wrapper only or also exercise the remote wrapper in a no-network-safe way.
- [Affects R9][Technical] Decide the test harness format and reporting style.
