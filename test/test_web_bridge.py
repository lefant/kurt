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
    vertex_to_point,
)


FINISHED_GAME = """
F4 C6 C7 H4 D6 E5 B7 G3 C5 E7 B6 E6 D5 F5 E4 G5 G4 H2 F6 H3
F7 H5 G7 E8 F8 D4 D8 C3 D7 F3 E9 H6 H7 B2 G6 D2 J6 C2 B5 E3
B4 F4 C4 A2 E2 A3 F2 C1 F1 D3 G1 J4 J3 G9 J1 J5 J2 D1 H1 G2
J7 C9 D9 C8 B8 G8 F9 A5 A4 J9 H8 H9 J8 A9 B9 E1 A8 F1 A6 G1
B3 G9 H9 C9 G8 B1 C8 J2 J1 H1 F2 E2 PASS PASS
""".split()


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

    def test_area_count_handles_unclaimed_shared_and_single_color_regions(self):
        board = Board(3)
        score = board.area_score()
        self.assertEqual(score["neutral"], 9)
        self.assertEqual(score["black"]["total"], 0)
        self.assertEqual(score["white"]["total"], 7.5)

        board.play("black", 0, 0)
        score = board.area_score()
        self.assertEqual(score["black"], {"stones": 1, "territory": 8, "total": 9})
        self.assertEqual(score["neutral"], 0)

        board.play("white", 2, 2)
        before = board.position()
        score = board.area_score()
        self.assertEqual(score["black"], {"stones": 1, "territory": 0, "total": 1})
        self.assertEqual(score["white"], {"stones": 1, "territory": 0, "total": 8.5})
        self.assertEqual(score["neutral"], 7)
        self.assertEqual(board.position(), before)


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
        for white_passes_first in (False, True):
            with self.subTest(white_passes_first=white_passes_first):
                client = FakeClient(["pass", "E5"])
                session = GameSession(lambda: client)
                revision = 0
                if white_passes_first:
                    revision = session.human_move(0, 0, revision)["revision"]
                state = session.human_move(None, None, revision)
                self.assertTrue(state["gameOver"])
                self.assertEqual(state["turn"], "none")
                self.assertEqual([move["vertex"] for move in state["moves"]][-2:], ["PASS", "PASS"])
                self.assertEqual(client.commands.count("genmove w"), 1)
                if white_passes_first:
                    self.assertEqual(state["score"]["black"]["total"], 81)
                    self.assertIn("Black wins by 73.5", state["status"])
                else:
                    self.assertEqual(state["score"]["neutral"], 81)
                    self.assertIn("White wins by 7.5", state["status"])

    def test_completed_game_counts_area_and_komi_not_captures(self):
        # This 94-move game was independently counted and checked against Kurt W+8.5.
        session = GameSession(lambda: FakeClient(FINISHED_GAME[1::2]))
        state = session.state()
        self.assertIsNone(state["score"])
        for vertex in FINISHED_GAME[::2]:
            row, col = (None, None) if vertex == "PASS" else vertex_to_point(vertex)
            state = session.human_move(row, col, state["revision"])
        self.assertEqual(len(state["moves"]), 94)
        self.assertEqual(state["score"], {
            "black": {"stones": 33, "territory": 7, "total": 40},
            "white": {"stones": 35, "territory": 6, "total": 48.5},
            "komi": 7.5,
            "neutral": 0,
        })
        self.assertIn("White wins by 8.5", state["status"])
        self.assertEqual(session.state()["score"], state["score"])

    def test_gtp_failure_stops_game_instead_of_leaving_it_thinking(self):
        for failed_command in ("play b A1", "genmove w"):
            with self.subTest(failed_command=failed_command):
                client = FakeClient()
                session = GameSession(lambda: client)
                command = client.command

                def fail(text):
                    if text == failed_command:
                        raise BridgeError("Kurt did not answer before the GTP timeout.")
                    return command(text)

                with patch.object(client, "command", side_effect=fail):
                    with self.assertRaisesRegex(BridgeError, "timeout"):
                        session.human_move(8, 0, 0)
                state = session.state()
                self.assertTrue(state["gameOver"])
                self.assertEqual(state["turn"], "none")
                self.assertIn("Start a new game", state["status"])
                self.assertIsNone(state["score"])
                self.assertTrue(client.closed)
                self.assertEqual(len(state["moves"]), int(failed_command == "genmove w"))
                self.assertEqual(state["board"][8][0], "black" if state["moves"] else None)

    def test_resignation_is_a_normal_game_result(self):
        client = FakeClient(["resign"])
        session = GameSession(lambda: client)
        state = session.human_move(4, 4, 0)
        self.assertEqual(state["moves"][-1], {"color": "white", "vertex": "RESIGN"})
        self.assertTrue(state["gameOver"])
        self.assertEqual(state["turn"], "none")
        self.assertEqual(state["status"], "Kurt resigned. You win.")
        self.assertIsNone(state["score"])
        self.assertFalse(client.closed)

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
        self.assertIsNone(second.state()["score"])
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
