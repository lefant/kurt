---
title: "Recover authoritative game state after a failed HTTP response"
date: 2026-11-07
category: integration-issues
module: "Kurt browser HTTP/GTP bridge"
problem_type: integration_issue
component: api_layer
severity: high
symptoms:
  - "A failed move response leaves the browser showing an older board than the server"
  - "A GTP timeout leaves the game permanently showing Kurt is thinking"
root_cause: async_timing
resolution_type: code_fix
tags: [gtp, http, timeout, state-recovery, browser, regression-test]
---

# Recover authoritative game state after a failed HTTP response

## Problem

A move request crosses two stateful boundaries: the browser sends an HTTP request
to the bridge, then the bridge sends GTP commands to Kurt. Losing the HTTP response
does not establish whether either command completed. Restoring the browser's old
board can hide a move that the engine already played.

## Symptoms

Two failures were reproduced independently:

- After an actual move completed, replacing its HTTP response with a 504 made the
  old browser code restore an empty board and zero moves. The server retained
  Black E5 and Kurt's reply.
- Forcing a GTP timeout during `genmove w` left the old session on White's turn,
  with no completed result and no way to continue playing.

The user's original incident was a 5xx, possibly 504, inside Amp's embedded portal
viewer, followed by Kurt resigning after a retry. Its exact request, response,
timing, and move sequence were unavailable. These reproductions establish bugs
in recovery, **not the cause of that original incident**.

## What Didn't Work

- Rolling back the displayed board only changes the browser. It cannot undo a
  GTP command or establish that the command was rejected.
- Testing resignation alone did not reproduce a crash. In a disposable real
  session, setting `komi -1000` before Black E5 produced a normal White resignation;
  the engine still answered `name` with `kurt`. Resignation and transport failure
  must be investigated separately.

## Solution

In [the browser request handlers](../../../web/static/app.js), both move and reset
failures call `recover(error)` instead of restoring previous state. The successful
recovery path reads the server before unlocking interaction:

```javascript
const nextState = await request('/api/state')
busy = false
render(nextState)
```

Keep the board and Pass disabled while that read is pending. If it fails too, keep
play disabled and offer reload or New game. Do not automatically replay the POST:
the response may have been lost after the move or reset completed. Preserve the
HTTP status when an upstream error body is HTML or plain text rather than JSON.

In [the bridge](../../../web/bridge.py), `GameSession.state()` and `human_move()`
share the session lock. A recovery read for a live session therefore waits for the
move operation to finish rather than observing it halfway through. Move requests
also carry a revision; stale requests are rejected before sending another GTP
command. Recovery requires the same browser session cookie and a surviving server
session; this is not persistence across a bridge restart or session expiration.

Treat a failed GTP exchange differently from a lost HTTP response. The bridge
cannot assume its board still matches the engine after a GTP timeout or protocol
error. It marks the game stopped, closes the engine, and requires a new game.
Validate Black's move on a copy of the board and commit it only after `play b`
is acknowledged. If `genmove w` then fails, retain the acknowledged Black move,
but do not invent White's reply or a numeric game result.

Flush request logs immediately and include elapsed time and errors. Handle a
disconnected HTTP client at the response-writing boundary: log the lost response
without attempting a second error response or changing the completed game.

## Why This Works

The browser learns what happened from the live bridge session, not from whether
its previous response arrived. The bridge, in turn, distinguishes acknowledged
GTP commands from an uncertain engine state. Neither layer can safely infer a
rollback from a timeout at the layer above it.

The browser's 30-second fetch timeout and the GTP client's 20-second response
timeout are separate limits. Kurt's configured 800 ms search budget is not an
HTTP deadline. Increasing a timeout alone would not fix stale-state recovery.

## Prevention

Test response loss **after** the server has changed state, not only failures
before a request reaches it. Keep the two failure boundaries separate:

- [Browser regressions](../../../test/test_web_ui.cjs) substitute an HTML 504
  after a committed move, verify recovery without replay, keep play locked when
  recovery fails, and recover a reset whose response was lost.
- [Bridge regressions](../../../test/test_web_bridge.py), especially
  `test_gtp_failure_stops_game_instead_of_leaving_it_thinking`, inject failures at
  both `play b` and `genmove w`. They check the stopped state, engine closure, and
  preservation of only acknowledged moves.

Run the focused checks from the repository root:

```sh
node --test test/test_web_ui.cjs
python3 -m unittest discover -s test -p 'test_web_bridge.py' -v
```

The implementation was also exercised with a real engine and browser. A client
that disconnected after 0.05 seconds did not cancel the move: the server finished
in about 0.805 seconds, and a later state read recovered both moves. Injected 504
responses after actual moves recovered the advanced board; a failed recovery read
left all board points and Pass disabled. A forced real GTP timeout stopped the
session and closed its process.

These checks do not reproduce Amp's embedded viewer failure. If it recurs, capture
the failed request URL, HTTP status/body, elapsed time, move sequence, and matching
bridge logs before resetting the game. Do not restart or mutate another player's
session to investigate; use a disposable session and close it afterward.
