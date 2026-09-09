#!/usr/bin/env python3
"""Dependency-free HTTP bridge between a browser and Kurt's GTP process."""

from __future__ import annotations

import argparse
import atexit
import json
import os
import re
import secrets
import selectors
import signal
import subprocess
import threading
import time
from collections import deque
from dataclasses import dataclass
from http import HTTPStatus
from http.cookies import SimpleCookie
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Callable
from urllib.parse import urlparse


ROOT = Path(__file__).resolve().parents[1]
STATIC_DIR = Path(__file__).resolve().parent / "static"
ENGINE_COMMAND = (str(ROOT / "scripts" / "kurt-gtp"),)
BOARD_SIZE = 9
BOARD_LETTERS = "ABCDEFGHJKLMNOPQRSTUVWXYZ"
SESSION_COOKIE = "kurt_session"
SESSION_TTL_SECONDS = 30 * 60
MAX_BODY_BYTES = 4096
MAX_GTP_FRAME_BYTES = 1024 * 1024


class BridgeError(Exception):
    """An error safe to show in the browser."""


class GtpProtocolError(BridgeError):
    pass


class GtpCommandError(BridgeError):
    pass


def parse_gtp_response(frame: bytes, expected_id: int) -> str:
    """Parse one complete GTP frame, including its terminating blank line."""
    normalized = frame.replace(b"\r\n", b"\n")
    if not normalized.endswith(b"\n\n"):
        raise GtpProtocolError("Kurt returned an unterminated GTP response.")
    if b"\n\n" in normalized[:-2]:
        raise GtpProtocolError("Kurt returned multiple GTP responses at once.")
    try:
        text = normalized[:-2].decode("utf-8")
    except UnicodeDecodeError as exc:
        raise GtpProtocolError("Kurt returned non-UTF-8 output.") from exc
    lines = text.split("\n")
    match = re.fullmatch(r"([=?])(\d+)(?: (.*))?", lines[0])
    if match is None:
        raise GtpProtocolError("Kurt returned malformed GTP response framing.")
    response_id = int(match.group(2))
    if response_id != expected_id:
        raise GtpProtocolError(
            f"Kurt returned response {response_id}, expected {expected_id}."
        )
    payload = "\n".join([match.group(3) or "", *lines[1:]]).rstrip("\n")
    if match.group(1) == "?":
        raise GtpCommandError(payload.strip() or "Kurt rejected the command.")
    return payload


class GtpClient:
    """Serialized, numbered GTP access to one repository-owned Kurt process."""

    def __init__(self) -> None:
        self._process = subprocess.Popen(
            ENGINE_COMMAND,
            cwd=ROOT,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            bufsize=0,
        )
        assert self._process.stdin is not None
        assert self._process.stdout is not None
        assert self._process.stderr is not None
        self._lock = threading.Lock()
        self._next_id = 1
        self._stdout_buffer = b""
        self._stderr_tail: deque[bytes] = deque(maxlen=40)
        self._closed = False
        self._stderr_thread = threading.Thread(target=self._drain_stderr, daemon=True)
        self._stderr_thread.start()

    def _drain_stderr(self) -> None:
        assert self._process.stderr is not None
        for line in iter(self._process.stderr.readline, b""):
            self._stderr_tail.append(line.rstrip())

    def command(self, command: str, timeout: float = 20.0) -> str:
        if not re.fullmatch(r"[\x20-\x7e]+", command):
            raise ValueError("GTP commands must be one printable ASCII line")
        with self._lock:
            if self._closed:
                raise BridgeError("The Kurt process is closed.")
            if self._process.poll() is not None:
                raise BridgeError(self._exit_message())
            command_id = self._next_id
            self._next_id += 1
            assert self._process.stdin is not None
            try:
                self._process.stdin.write(f"{command_id} {command}\n".encode("ascii"))
                self._process.stdin.flush()
            except (BrokenPipeError, OSError) as exc:
                raise BridgeError(self._exit_message()) from exc
            frame = self._read_frame(timeout)
            return parse_gtp_response(frame, command_id)

    def _read_frame(self, timeout: float) -> bytes:
        assert self._process.stdout is not None
        deadline = time.monotonic() + timeout
        selector = selectors.DefaultSelector()
        selector.register(self._process.stdout, selectors.EVENT_READ)
        try:
            while b"\n\n" not in self._stdout_buffer.replace(b"\r\n", b"\n"):
                remaining = deadline - time.monotonic()
                if remaining <= 0 or not selector.select(remaining):
                    raise BridgeError("Kurt did not answer before the GTP timeout.")
                chunk = os.read(self._process.stdout.fileno(), 4096)
                if not chunk:
                    raise BridgeError(self._exit_message())
                self._stdout_buffer += chunk
                if len(self._stdout_buffer) > MAX_GTP_FRAME_BYTES:
                    raise GtpProtocolError("Kurt returned an oversized GTP response.")
            normalized = self._stdout_buffer.replace(b"\r\n", b"\n")
            boundary = normalized.index(b"\n\n") + 2
            frame = normalized[:boundary]
            self._stdout_buffer = normalized[boundary:]
            return frame
        finally:
            selector.close()

    def _exit_message(self) -> str:
        detail = b"\n".join(self._stderr_tail).decode("utf-8", errors="replace").strip()
        suffix = f" Last output: {detail[-400:]}" if detail else ""
        return f"Kurt exited unexpectedly.{suffix}"

    def close(self) -> None:
        with self._lock:
            if self._closed:
                return
            self._closed = True
            if self._process.poll() is None:
                assert self._process.stdin is not None
                try:
                    self._process.stdin.write(f"{self._next_id} quit\n".encode("ascii"))
                    self._process.stdin.flush()
                    self._process.wait(timeout=2)
                except (BrokenPipeError, OSError, subprocess.TimeoutExpired):
                    self._process.terminate()
                    try:
                        self._process.wait(timeout=2)
                    except subprocess.TimeoutExpired:
                        self._process.kill()
                        self._process.wait(timeout=2)
            for stream in (self._process.stdin, self._process.stdout, self._process.stderr):
                if stream is not None:
                    stream.close()


class Board:
    def __init__(self, size: int = BOARD_SIZE) -> None:
        self.size = size
        self.grid: list[list[str | None]] = [[None] * size for _ in range(size)]
        self.history = [self.position()]

    def position(self) -> tuple[tuple[str | None, ...], ...]:
        return tuple(tuple(row) for row in self.grid)

    def neighbors(self, row: int, col: int):
        for next_row, next_col in (
            (row - 1, col),
            (row + 1, col),
            (row, col - 1),
            (row, col + 1),
        ):
            if 0 <= next_row < self.size and 0 <= next_col < self.size:
                yield next_row, next_col

    def group(self, row: int, col: int) -> tuple[set[tuple[int, int]], set[tuple[int, int]]]:
        color = self.grid[row][col]
        stones = {(row, col)}
        liberties: set[tuple[int, int]] = set()
        pending = [(row, col)]
        while pending:
            current_row, current_col = pending.pop()
            for point in self.neighbors(current_row, current_col):
                value = self.grid[point[0]][point[1]]
                if value is None:
                    liberties.add(point)
                elif value == color and point not in stones:
                    stones.add(point)
                    pending.append(point)
        return stones, liberties

    def play(self, color: str, row: int, col: int) -> int:
        if color not in {"black", "white"}:
            raise ValueError("unknown stone color")
        if not (0 <= row < self.size and 0 <= col < self.size):
            raise BridgeError("That point is outside the board.")
        if self.grid[row][col] is not None:
            raise BridgeError("That point is already occupied.")

        previous = [line[:] for line in self.grid]
        self.grid[row][col] = color
        opponent = "white" if color == "black" else "black"
        captured: set[tuple[int, int]] = set()
        for next_row, next_col in self.neighbors(row, col):
            if self.grid[next_row][next_col] == opponent:
                stones, liberties = self.group(next_row, next_col)
                if not liberties:
                    captured.update(stones)
        for captured_row, captured_col in captured:
            self.grid[captured_row][captured_col] = None

        _, liberties = self.group(row, col)
        if not liberties:
            self.grid = previous
            raise BridgeError("That move is suicide.")
        new_position = self.position()
        if len(self.history) >= 2 and new_position == self.history[-2]:
            self.grid = previous
            raise BridgeError("That move repeats the previous board position (ko).")
        self.history.append(new_position)
        return len(captured)

    def pass_turn(self) -> None:
        self.history.append(self.position())


def point_to_vertex(row: int, col: int, size: int = BOARD_SIZE) -> str:
    return f"{BOARD_LETTERS[col]}{size - row}"


def vertex_to_point(vertex: str, size: int = BOARD_SIZE) -> tuple[int, int]:
    match = re.fullmatch(r"([A-HJ-Z])(\d+)", vertex.upper())
    if match is None or match.group(1) not in BOARD_LETTERS:
        raise GtpProtocolError(f"Kurt returned invalid move {vertex!r}.")
    col = BOARD_LETTERS.index(match.group(1))
    row = size - int(match.group(2))
    if not (0 <= row < size and 0 <= col < size):
        raise GtpProtocolError(f"Kurt returned out-of-board move {vertex!r}.")
    return row, col


@dataclass
class Move:
    color: str
    vertex: str


class GameSession:
    def __init__(self, client_factory: Callable[[], GtpClient] = GtpClient) -> None:
        self.lock = threading.Lock()
        self.client = client_factory()
        self.board = Board()
        self.turn = "black"
        self.status = "Your turn — play Black."
        self.moves: list[Move] = []
        self.consecutive_passes = 0
        self.game_over = False
        self.revision = 0
        self.last_seen = time.monotonic()
        try:
            for command in (
                "boardsize 9",
                "komi 7.5",
                "kurt_configure maxplayouts 1000",
                "kurt_configure maxtime 800",
                "clear_board",
            ):
                self.client.command(command)
        except Exception:
            self.client.close()
            raise

    def _state(self) -> dict[str, object]:
        self.last_seen = time.monotonic()
        return {
            "size": self.board.size,
            "board": self.board.grid,
            "turn": self.turn,
            "status": self.status,
            "moves": [move.__dict__ for move in self.moves],
            "gameOver": self.game_over,
            "revision": self.revision,
        }

    def state(self) -> dict[str, object]:
        with self.lock:
            return self._state()

    def human_move(self, row: int | None, col: int | None, revision: int) -> dict[str, object]:
        with self.lock:
            if revision != self.revision:
                raise BridgeError("The game changed. Use the current board and try again.")
            if self.game_over:
                raise BridgeError("This game is over. Start a new game to continue.")
            if self.turn != "black":
                raise BridgeError("Wait for Kurt to finish its move.")

            if row is None or col is None:
                vertex = "pass"
                self.board.pass_turn()
                self.consecutive_passes += 1
            else:
                if type(row) is not int or type(col) is not int:
                    raise BridgeError("Move coordinates must be whole numbers.")
                self.board.play("black", row, col)
                vertex = point_to_vertex(row, col)
                self.consecutive_passes = 0

            self.client.command(f"play b {vertex}")
            self.moves.append(Move("black", vertex.upper()))
            self.turn = "white"
            self.status = "Kurt is thinking…"
            self.revision += 1

            response = self.client.command("genmove w").strip().lower()
            if response == "resign":
                self.moves.append(Move("white", "RESIGN"))
                self.game_over = True
                self.turn = "none"
                self.status = "Kurt resigned. You win."
            elif response == "pass":
                self.board.pass_turn()
                self.moves.append(Move("white", "PASS"))
                self.consecutive_passes += 1
                if self.consecutive_passes >= 2:
                    self.game_over = True
                    self.turn = "none"
                    self.status = "Both players passed. Game over."
                else:
                    self.turn = "black"
                    self.status = "Kurt passed. Your turn."
            else:
                kurt_row, kurt_col = vertex_to_point(response)
                self.board.play("white", kurt_row, kurt_col)
                self.moves.append(Move("white", response.upper()))
                self.consecutive_passes = 0
                self.turn = "black"
                self.status = f"Kurt played {response.upper()}. Your turn."
            self.revision += 1
            return self._state()

    def close(self) -> None:
        self.client.close()


class SessionRegistry:
    def __init__(self, client_factory: Callable[[], GtpClient] = GtpClient) -> None:
        self._client_factory = client_factory
        self._sessions: dict[str, GameSession] = {}
        self._lock = threading.Lock()

    def get(self, session_id: str) -> GameSession:
        with self._lock:
            session = self._sessions.get(session_id)
            if session is None:
                session = GameSession(self._client_factory)
                self._sessions[session_id] = session
            session.last_seen = time.monotonic()
            return session

    def reset(self, session_id: str) -> GameSession:
        with self._lock:
            old = self._sessions.pop(session_id, None)
            if old is not None:
                old.close()
            session = GameSession(self._client_factory)
            self._sessions[session_id] = session
            return session

    def remove(self, session_id: str) -> None:
        with self._lock:
            session = self._sessions.pop(session_id, None)
        if session is not None:
            session.close()

    def reap(self) -> None:
        cutoff = time.monotonic() - SESSION_TTL_SECONDS
        with self._lock:
            expired = [key for key, value in self._sessions.items() if value.last_seen < cutoff]
        for session_id in expired:
            self.remove(session_id)

    def close(self) -> None:
        with self._lock:
            sessions = list(self._sessions.values())
            self._sessions.clear()
        for session in sessions:
            session.close()


REGISTRY = SessionRegistry()


class BridgeHandler(BaseHTTPRequestHandler):
    server_version = "KurtBridge/1.0"

    def log_message(self, format: str, *args: object) -> None:
        print(f"{self.address_string()} - {format % args}")

    def do_GET(self) -> None:
        path = urlparse(self.path).path
        if path == "/healthz":
            self._json({"ok": True})
        elif path == "/api/state":
            self._api(lambda session: session.state())
        elif path == "/":
            self._static("index.html", "text/html; charset=utf-8")
        elif path == "/app.css":
            self._static("app.css", "text/css; charset=utf-8")
        elif path == "/app.js":
            self._static("app.js", "text/javascript; charset=utf-8")
        else:
            self.send_error(HTTPStatus.NOT_FOUND)

    def do_POST(self) -> None:
        path = urlparse(self.path).path
        if path == "/api/reset":
            session_id, is_new = self._session_id()
            try:
                state = REGISTRY.reset(session_id).state()
                self._json(state, cookie=session_id if is_new else None)
            except BridgeError as exc:
                self._json({"error": str(exc)}, status=HTTPStatus.BAD_GATEWAY)
        elif path == "/api/move":
            def move(session: GameSession):
                body = self._body()
                return session.human_move(body.get("row"), body.get("col"), body.get("revision"))
            self._api(move)
        elif path == "/api/close":
            session_id, _ = self._session_id()
            REGISTRY.remove(session_id)
            self._json({"ok": True})
        else:
            self.send_error(HTTPStatus.NOT_FOUND)

    def _api(self, action: Callable[[GameSession], dict[str, object]]) -> None:
        session_id, is_new = self._session_id()
        try:
            result = action(REGISTRY.get(session_id))
            self._json(result, cookie=session_id if is_new else None)
        except (BridgeError, TypeError, ValueError) as exc:
            self._json({"error": str(exc)}, status=HTTPStatus.CONFLICT)
        except Exception as exc:
            print(f"bridge error: {exc}")
            self._json({"error": "The bridge hit an unexpected error."}, status=HTTPStatus.INTERNAL_SERVER_ERROR)

    def _session_id(self) -> tuple[str, bool]:
        cookie = SimpleCookie(self.headers.get("Cookie", ""))
        morsel = cookie.get(SESSION_COOKIE)
        if morsel is not None and re.fullmatch(r"[A-Za-z0-9_-]{32,64}", morsel.value):
            return morsel.value, False
        return secrets.token_urlsafe(32), True

    def _body(self) -> dict:
        try:
            length = int(self.headers.get("Content-Length", "0"))
        except ValueError as exc:
            raise BridgeError("Invalid request length.") from exc
        if length <= 0 or length > MAX_BODY_BYTES:
            raise BridgeError("Invalid request body.")
        try:
            value = json.loads(self.rfile.read(length))
        except (json.JSONDecodeError, UnicodeDecodeError) as exc:
            raise BridgeError("Invalid JSON request.") from exc
        if not isinstance(value, dict):
            raise BridgeError("Request body must be an object.")
        return value

    def _json(self, body: object, status: HTTPStatus = HTTPStatus.OK, cookie: str | None = None) -> None:
        encoded = json.dumps(body).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(encoded)))
        self.send_header("Cache-Control", "no-store")
        if cookie is not None:
            self.send_header("Set-Cookie", f"{SESSION_COOKIE}={cookie}; Path=/; HttpOnly; SameSite=Lax")
        self.end_headers()
        self.wfile.write(encoded)

    def _static(self, name: str, content_type: str) -> None:
        body = (STATIC_DIR / name).read_bytes()
        self.send_response(HTTPStatus.OK)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-cache")
        self.send_header("X-Content-Type-Options", "nosniff")
        self.end_headers()
        self.wfile.write(body)


def serve(host: str, port: int) -> None:
    server = ThreadingHTTPServer((host, port), BridgeHandler)
    server.daemon_threads = True
    stop = threading.Event()

    def reap_sessions() -> None:
        while not stop.wait(60):
            REGISTRY.reap()

    threading.Thread(target=reap_sessions, daemon=True).start()
    atexit.register(REGISTRY.close)

    def shutdown(_signum, _frame) -> None:
        threading.Thread(target=server.shutdown, daemon=True).start()

    signal.signal(signal.SIGTERM, shutdown)
    signal.signal(signal.SIGINT, shutdown)
    print(f"Kurt web bridge listening on {host}:{port}", flush=True)
    try:
        server.serve_forever()
    finally:
        stop.set()
        server.server_close()
        REGISTRY.close()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=int(os.environ.get("PORT", "8080")))
    args = parser.parse_args()
    serve(args.host, args.port)


if __name__ == "__main__":
    main()
