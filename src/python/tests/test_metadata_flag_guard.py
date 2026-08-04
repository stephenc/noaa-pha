"""An explicitly-passed --mshr-zip/--phr-zip that is missing must abort.

Auto-discovery is allowed to find nothing (it only assigns a path that already
exists), but a path typed on the command line and not found used to fall
through to "no evidence" and still exit 0 -- which silently produced a whole
reconstruction pass with no partial-month evidence for the CONUS solve and no
non-CONUS metadata histories at all.
"""

import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import reconstruct_his  # noqa: E402


class _StopHere(Exception):
    """Sentinel: reached the work that follows the metadata guard."""


class MetadataFlagGuardTest(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.base = Path(self._tmp.name)
        self.addCleanup(self._tmp.cleanup)

    def _run(self, *extra: str):
        argv = ["reconstruct_his.py", "--base", str(self.base), *extra]
        with mock.patch.object(sys, "argv", argv):
            return reconstruct_his.main()

    def _assert_rejects(self, flag: str, value: str) -> None:
        with self.assertRaises(SystemExit) as ctx:
            self._run(flag, value)
        # A bare string exit code means failure with that message.
        self.assertNotIn(ctx.exception.code, (0, None))
        self.assertIn(flag, str(ctx.exception.code))

    def test_missing_mshr_zip_aborts(self) -> None:
        self._assert_rejects("--mshr-zip", str(self.base / "nope.zip"))

    def test_missing_phr_zip_aborts(self) -> None:
        self._assert_rejects("--phr-zip", str(self.base / "nope.zip"))

    def test_directory_is_not_accepted_as_a_zip(self) -> None:
        # is_file(), not exists(): a directory must not satisfy the flag.
        d = self.base / "adir"
        d.mkdir()
        self._assert_rejects("--mshr-zip", str(d))

    def test_existing_file_passes_the_guard(self) -> None:
        # Content is irrelevant here; the guard only checks presence, and we
        # stop at the next stage so no solve is attempted.
        present = self.base / "mshr_enhanced.txt.zip"
        present.write_bytes(b"not really a zip")
        with mock.patch.object(
            reconstruct_his, "_select_stations", side_effect=_StopHere
        ):
            with self.assertRaises(_StopHere):
                self._run("--mshr-zip", str(present))

    def test_absent_flags_do_not_abort(self) -> None:
        # No flags and no discoverable metadata: still allowed (warn + continue).
        with mock.patch.object(
            reconstruct_his, "_select_stations", side_effect=_StopHere
        ):
            with self.assertRaises(_StopHere):
                self._run()


if __name__ == "__main__":
    unittest.main()
