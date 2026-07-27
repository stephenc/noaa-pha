"""Synthetic round-trip tests for the Phase-5 verification gate.

Everything is fabricated through the exact forward model into a temp
workspace laid out like the real one; the tobo provider is injected so no
TOBMain run is needed.
"""

import json
import random
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import fp32  # noqa: E402
import ghcn_io  # noqa: E402
import verify_his  # noqa: E402

OFF_17 = {
    1: -6,
    2: -6,
    3: -7,
    4: -6,
    5: -5,
    6: -4,
    7: -3,
    8: -3,
    9: -4,
    10: -5,
    11: -5,
    12: -6,
}
OFF_07 = {1: 4, 2: 4, 3: 4, 4: 1, 5: 1, 6: 1, 7: 0, 8: -1, 9: -1, 10: 1, 11: 1, 12: 4}

SID = "USQVERIFY01"
SWITCH = (1975, 6)
CP = (1962, 1)
S1 = fp32.f32(0.61)


def month_range(y0, m0, y1, m1):
    out = []
    y, m = y0, m0
    while (y, m) <= (y1, m1):
        out.append((y, m))
        m += 1
        if m == 13:
            y, m = y + 1, 1
    return out


YMS = month_range(1950, 1, 2009, 12)


def _tobo(y, m):
    return OFF_17[m] if (y, m) < SWITCH else OFF_07[m]


def _s(y, m):
    return S1 if (y, m) < CP else 0.0


class _Workspace:
    """Fabricated station workspace with injectable corruption."""

    def __init__(self, td: Path, corrupt_qcf=None, corrupt_tout=None):
        self.tob_dir = td / "tob"
        self.qcu_dir = td / "qcu"
        self.qcf_dir = td / "qcf"
        self.solutions = td / "solutions"
        for d in (self.tob_dir, self.qcu_dir, self.qcf_dir, self.solutions):
            d.mkdir(parents=True)

        rng = random.Random(99)
        qcu, t_out, qcf = {}, {}, {}
        for y, m in YMS:
            v = rng.randint(-300, 3200)
            qcu[(y, m)] = v
            t = v + _tobo(y, m)
            t_out[(y, m)] = t
        if corrupt_tout:
            t_out[corrupt_tout] += 1
        for ym, t in t_out.items():
            qcf[ym] = fp32.pha_qcf(t, _s(*ym))
        if corrupt_qcf:
            qcf[corrupt_qcf] += 2

        self._write(self.qcu_dir / f"{SID}.raw.tavg", qcu)
        self._write(self.tob_dir / f"{SID}.tob.tavg", t_out)
        self._write(self.qcf_dir / f"{SID}.qcf.tavg", qcf)

        seg1 = fp32.solve_segment(
            [(t_out[ym], qcf[ym]) for ym in YMS if ym < CP and ym != corrupt_qcf]
        )
        seg2 = fp32.solve_segment(
            [(t_out[ym], qcf[ym]) for ym in YMS if ym >= CP and ym != corrupt_qcf]
        )
        sol = {
            "station_id": SID,
            "kind": "tob",
            "coord_index": 0,
            "regimes": [
                {"begin": [1950, 1, 1], "end": None, "code": "17HR", "blend_day": None},
                {
                    "begin": [SWITCH[0], SWITCH[1], 1],
                    "end": None,
                    "code": "07HR",
                    "blend_day": None,
                },
            ],
            "segments": [
                {
                    "begin": [1950, 1],
                    "end": [CP[0] - 1, 12],
                    "s_interval": [seg1.lo, seg1.hi],
                    "n_constraints": 0,
                    "visible": 144,
                },
                {
                    "begin": list(CP),
                    "end": [2009, 12],
                    "s_interval": [seg2.lo, seg2.hi],
                    "n_constraints": 0,
                    "visible": 576,
                },
            ],
            "deviants": [],
            "knife_edges": [],
            "audits": [],
            "cost": [0, 0, 1, 1, 0],
            "exact": True,
            "stats": {},
        }
        with open(self.solutions / f"{SID}.json", "w") as fh:
            json.dump(sol, fh)

        self.inv = {SID: ghcn_io.Inv(SID, 40.0, -90.0, 200.0, "SYNTH")}

    @staticmethod
    def _write(path, values):
        sd = ghcn_io.StationData(station_id=SID)
        for (y, m), v in values.items():
            sd.values[(y, m)] = v
            sd.flags[(y, m)] = "   "
            if y not in sd.year_list:
                sd.year_list.append(y)
        ghcn_io.write_station_data(path, sd)

    def provider(self, sid, coord, knife_edges):
        offsets = {
            "17HR": {(y, m): OFF_17[m] for (y, m) in YMS},
            "07HR": {(y, m): OFF_07[m] for (y, m) in YMS},
        }
        for code, mo, delta in knife_edges or []:
            for ym in list(offsets.get(code, {})):
                if ym[1] == mo:
                    offsets[code][ym] += delta
        return offsets

    def verify(self):
        return verify_his.verify_station(
            SID,
            self.tob_dir,
            self.qcu_dir,
            self.qcf_dir,
            self.solutions,
            self.inv,
            self.provider,
        )


class TestVerify(unittest.TestCase):
    def test_clean_roundtrip_passes(self):
        with tempfile.TemporaryDirectory() as td:
            ws = _Workspace(Path(td))
            res = ws.verify()
        self.assertEqual(res.cls, "tob")
        self.assertTrue(res.exact_pha)
        self.assertTrue(res.exact_tob)
        self.assertEqual(res.n_mismatch, 0)
        self.assertFalse(res.resegmented)

    def test_corrupt_qcf_month_caught(self):
        with tempfile.TemporaryDirectory() as td:
            ws = _Workspace(Path(td), corrupt_qcf=(1990, 7))
            res = ws.verify()
        self.assertFalse(res.exact_pha)
        self.assertTrue(res.exact_tob)  # .his side unaffected
        self.assertGreaterEqual(res.n_mismatch, 1)
        self.assertTrue(any(s.startswith("pha@1990-07") for s in res.samples))

    def test_corrupt_his_offset_caught_as_tob(self):
        # t_out wrong for one month, but qcf recomputed FROM the corrupted
        # t_out: PHA side is self-consistent, only the TOB equality catches
        # the emission infidelity.
        with tempfile.TemporaryDirectory() as td:
            ws = _Workspace(Path(td), corrupt_tout=(1985, 3))
            res = ws.verify()
        self.assertFalse(res.exact_tob)
        self.assertTrue(res.exact_pha)
        self.assertEqual(res.n_mismatch, 1)
        self.assertTrue(any(s.startswith("tob@1985-03") for s in res.samples))

    def test_verbatim_station(self):
        with tempfile.TemporaryDirectory() as td:
            ws = _Workspace(Path(td))
            # no-solution station: t_out must equal qcu
            (ws.solutions / f"{SID}.json").unlink()
            qcu = ghcn_io.read_station_data(ws.qcu_dir / f"{SID}.raw.tavg")
            ghcn_io.write_station_data(ws.tob_dir / f"{SID}.tob.tavg", qcu)
            res = ws.verify()
        self.assertEqual(res.cls, "verbatim")
        self.assertTrue(res.exact_tob)
        self.assertIsNone(res.exact_pha)

    def test_verbatim_difference_flagged(self):
        with tempfile.TemporaryDirectory() as td:
            ws = _Workspace(Path(td))
            (ws.solutions / f"{SID}.json").unlink()
            res = ws.verify()  # t_out has TOB offsets applied != qcu
        self.assertEqual(res.cls, "verbatim")
        self.assertFalse(res.exact_tob)
        self.assertGreater(res.n_mismatch, 0)

    def test_stale_interval_resegmented(self):
        with tempfile.TemporaryDirectory() as td:
            ws = _Workspace(Path(td))
            sol_path = ws.solutions / f"{SID}.json"
            sol = json.loads(sol_path.read_text())
            # widen the first segment's interval so its endpoints disagree
            lo, hi = sol["segments"][0]["s_interval"]
            sol["segments"][0]["s_interval"] = [lo - 0.005, hi + 0.005]
            sol_path.write_text(json.dumps(sol))
            res = ws.verify()
        self.assertTrue(res.resegmented)
        self.assertTrue(res.exact_pha)
        self.assertEqual(res.n_mismatch, 0)


if __name__ == "__main__":
    unittest.main()
