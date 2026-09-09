import io
import sys
import unittest
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "web"))

from bridge import (  # noqa: E402
    Board,
    BridgeError,
    ENGINE_COMMAND,
    GameSession,
    GtpClient,
    GtpCommandError,
    GtpProtocolError,
    SessionRegistry,
    parse_gtp_response,
)


class MemoryStream(io.BytesIO):
    def close(self):
        pass


class FakeProcess:
    def __init__(self, returncode=None):
        self.stdin = MemoryStream()
        self.stdout = MemoryStream()
        self.stderr = MemoryStream()
        self.returncode = returncode
        self.terminated = False

    def poll(self):
        return self.returncode

    def wait(self, timeout=None):
        self.returncode = 0
        return 0

    def terminate(self):
        self.terminated = True
        self.returncode = -15

    def kill(self):
        self.returncode = -9


class FakeClient:
    def __init__(self, moves=None):
        self.commands = []
        self.moves = iter(moves or ["E5"])
        self.closed = False

    def command(self, command, timeout=20.0):
        if self.closed:
            raise AssertionError("command sent to closed engine")
        self.commands.append(command)
        return next(self.moves) if command == "genmove w" else ""

    def close(self):
        self.closed = True


class GtpFramingTests(unittest.TestCase):
    def test_parses_numbered_multiline_crlf_response(self):
        self.assertEqual(parse_gtp_response(b"=17 first\r\nsecond\r\n\r\n", 17), "first\nsecond")

    def test_rejects_missing_terminator_and_wrong_id(self):
        with self.assertRaisesRegex(GtpProtocolError, "unterminated"):
            parse_gtp_response(b"=3 ok\n", 3)
        with self.assertRaisesRegex(GtpProtocolError, "expected 3"):
            parse_gtp_response(b"=4 ok\n\n", 3)

    def test_rejects_malformed_prefix_and_surfaces_gtp_errors(self):
        with self.assertRaisesRegex(GtpProtocolError, "malformed"):
            parse_gtp_response(b"debug output\n\n", 2)
        with self.assertRaisesRegex(GtpCommandError, "illegal move"):
            parse_gtp_response(b"?2 illegal move\n\n", 2)

    def test_client_starts_only_repository_wrapper_and_quits_on_close(self):
        process = FakeProcess()
        with patch("bridge.subprocess.Popen", return_value=process) as popen:
            client = GtpClient()
            client.close()
        self.assertEqual(popen.call_args.args, (ENGINE_COMMAND,))
        self.assertNotIn("shell", popen.call_args.kwargs)
        self.assertEqual(popen.call_args.kwargs["cwd"], ROOT)
        self.assertEqual(process.stdin.getvalue(), b"1 quit\n")
        self.assertEqual(process.returncode, 0)

    def test_client_reports_an_engine_that_already_exited(self):
        process = FakeProcess(returncode=1)
        with patch("bridge.subprocess.Popen", return_value=process):
            client = GtpClient()
            with self.assertRaisesRegex(BridgeError, "exited unexpectedly"):
                client.command("name")
            client.close()


class BoardTests(unittest.TestCase):
    def test_rejects_occupied_and_suicide_moves(self):
        board = Board(3)
        board.play("black", 1, 1)
        with self.assertRaisesRegex(BridgeError, "occupied"):
            board.play("white", 1, 1)

        suicide = Board(3)
        for row, col in [(0, 1), (1, 0), (1, 2), (2, 1)]:
            suicide.play("black", row, col)
        with self.assertRaisesRegex(BridgeError, "suicide"):
            suicide.play("white", 1, 1)
        self.assertIsNone(suicide.grid[1][1])

    def test_captures_a_surrounded_group(self):
        board = Board(3)
        board.play("white", 1, 1)
        for row, col in [(0, 1), (1, 0), (1, 2)]:
            board.play("black", row, col)
        self.assertEqual(board.play("black", 2, 1), 1)
        self.assertIsNone(board.grid[1][1])


class SessionTests(unittest.TestCase):
    def test_move_is_validated_then_serialized_with_engine_reply(self):
        client = FakeClient(["C7"])
        session = GameSession(lambda: client)
        state = session.human_move(8, 0, 0)
        self.assertEqual(client.commands[-2:], ["play b A1", "genmove w"])
        self.assertEqual(state["board"][8][0], "black")
        self.assertEqual(state["board"][2][2], "white")
        self.assertEqual(state["turn"], "black")

    def test_rejects_out_of_turn_and_stale_requests_before_gtp(self):
        client = FakeClient()
        session = GameSession(lambda: client)
        setup_count = len(client.commands)
        session.turn = "white"
        with self.assertRaisesRegex(BridgeError, "Wait for Kurt"):
            session.human_move(0, 0, 0)
        session.turn = "black"
        with self.assertRaisesRegex(BridgeError, "game changed"):
            session.human_move(0, 0, 99)
        self.assertEqual(len(client.commands), setup_count)

    def test_illegal_move_never_reaches_engine(self):
        client = FakeClient()
        session = GameSession(lambda: client)
        session.board.grid[0][0] = "white"
        setup_count = len(client.commands)
        with self.assertRaisesRegex(BridgeError, "occupied"):
            session.human_move(0, 0, 0)
        self.assertEqual(len(client.commands), setup_count)

    def test_pass_and_second_pass_end_game(self):
        client = FakeClient(["pass"])
        session = GameSession(lambda: client)
        state = session.human_move(None, None, 0)
        self.assertTrue(state["gameOver"])
        self.assertEqual([move["vertex"] for move in state["moves"]], ["PASS", "PASS"])

    def test_reset_and_remove_close_exact_engine_instances(self):
        clients = []

        def factory():
            client = FakeClient()
            clients.append(client)
            return client

        registry = SessionRegistry(factory)
        first = registry.get("session")
        second = registry.reset("session")
        self.assertIsNot(first, second)
        self.assertTrue(clients[0].closed)
        self.assertFalse(clients[1].closed)
        registry.remove("session")
        self.assertTrue(clients[1].closed)

    def test_browser_sessions_get_isolated_engines(self):
        clients = []

        def factory():
            client = FakeClient()
            clients.append(client)
            return client

        registry = SessionRegistry(factory)
        first = registry.get("first")
        second = registry.get("second")
        self.assertIsNot(first, second)
        self.assertIsNot(first.client, second.client)
        registry.close()
        self.assertTrue(all(client.closed for client in clients))


if __name__ == "__main__":
    unittest.main()
