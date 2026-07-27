import sys
import tempfile
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO / "src" / "python"))

import tob_basis  # noqa: E402
import fp32  # noqa: E402
from tob_basis import (  # noqa: E402
    BASIS_CODES,
    Basis,
    BasisRunner,
    _consensus,
    _day_before,
    _his_row,
    _station_adjusted,
    read_station_values,
)

TOB_BIN = REPO / "bin" / "TOBMain"
INV = REPO / "data" / "input" / "station.inv"
RAW_DIR = REPO / "data" / "input" / "raw" / "tavg"

HAVE_DATA = TOB_BIN.exists() and INV.exists() and RAW_DIR.is_dir()


def _pick_conus_station():
    """First US station inside the CONUS box with a substantial raw file."""
    with open(INV) as fh:
        for line in fh:
            sid = line[:11].strip()
            if not sid.startswith("US"):
                continue
            lat = float(line[12:20])
            lon = float(line[21:30])
            if not (23.0 <= lat <= 50.0 and -126.0 <= lon <= -65.0):
                continue
            raw = RAW_DIR / f"{sid}.raw.tavg"
            if raw.exists() and raw.stat().st_size > 5000:
                return sid, lat, lon, raw
    return None


class TestHisRow(unittest.TestCase):
    def test_column_positions(self):
        row = _his_row("USB0001XXXX", 0, (1948, 7, 1), (9999, 12, 31), "18HR")
        self.assertEqual(row[0], "0")
        self.assertEqual(row[1:12], "USB0001XXXX")
        self.assertEqual(row[14:18], "1948")  # cols 15-18
        self.assertEqual(row[18:20], "07")  # cols 19-20
        self.assertEqual(row[20:22], "01")  # cols 21-22
        self.assertEqual(row[23:27], "9999")  # cols 24-27
        self.assertEqual(row[27:29], "12")  # cols 28-29
        self.assertEqual(row[29:31], "31")  # cols 30-31
        self.assertEqual(row[77:81], "18HR")  # cols 78-81
        # everything else blank
        self.assertEqual(row[31:77].strip(), "")
        self.assertEqual(row[81:].strip(), "")

    def test_special_codes_fit(self):
        for code in ["00RS", "00SR", "00SS", "24HR"]:
            row = _his_row("USB0000XXXX", 0, (1900, 1, 1), (9999, 12, 31), code)
            self.assertEqual(row[77:81], code)


class TestHelpers(unittest.TestCase):
    def test_consensus_flags_disagreement(self):
        by_ym = {
            (1950, 1): -5,
            (1951, 1): -5,
            (1952, 1): -4,
            (1950, 2): -3,
            (1951, 2): -3,
        }
        cons, anoms = _consensus(by_ym)
        self.assertEqual(cons[1], -5)
        self.assertEqual(cons[2], -3)
        self.assertEqual(len(anoms), 1)
        m, counts = anoms[0]
        self.assertEqual(m, 1)
        self.assertEqual(counts, {-5: 2, -4: 1})

    def test_station_adjusted_detection(self):
        # verbatim-copy run (mict < 5 or out-of-gate coord): every code zero
        all_zero = {code: {(1950, m): 0 for m in range(1, 13)} for code in BASIS_CODES}
        self.assertFalse(_station_adjusted(all_zero))
        # a single nonzero offset anywhere marks the station adjusted
        adjusted = {code: {(1950, m): 0 for m in range(1, 13)} for code in BASIS_CODES}
        adjusted["07HR"][(1950, 1)] = -5
        self.assertTrue(_station_adjusted(adjusted))

    def test_station_adjusted_json_roundtrip(self):
        b = Basis("USC00000000", (40.0, -90.0))
        b.station_adjusted = False
        b2 = Basis.from_json(b.to_json())
        self.assertFalse(b2.station_adjusted)
        # legacy payloads without the field default to True
        payload = b.to_json()
        del payload["station_adjusted"]
        self.assertTrue(Basis.from_json(payload).station_adjusted)

    def test_day_before(self):
        self.assertEqual(_day_before(1960, 6, 15), (1960, 6, 14))
        self.assertEqual(_day_before(1960, 6, 1), (1960, 5, 31))
        self.assertEqual(_day_before(1960, 3, 1), (1960, 2, 29))
        self.assertEqual(_day_before(1961, 3, 1), (1961, 2, 28))
        self.assertEqual(_day_before(1960, 1, 1), (1959, 12, 31))

    def test_basis_json_roundtrip(self):
        b = Basis("USC00000000", (40.0, -90.0))
        b.code_offsets = {"07HR": {1: -5, 2: -4}}
        b.code_offsets_by_ym = {"07HR": {(1950, 1): -5, (1951, 2): -4}}
        b.anomalies = [("07HR", 1, {-5: 3, -4: 1})]
        b2 = Basis.from_json(b.to_json())
        self.assertEqual(b2.code_offsets, b.code_offsets)
        self.assertEqual(b2.code_offsets_by_ym, b.code_offsets_by_ym)
        self.assertEqual(b2.anomalies, b.anomalies)


@unittest.skipUnless(HAVE_DATA, "bin/TOBMain or data/ not present")
class TestIntegration(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        picked = _pick_conus_station()
        assert picked, "no CONUS station found in inventory"
        cls.sid, cls.lat, cls.lon, cls.raw = picked
        cls.tmp = tempfile.TemporaryDirectory(prefix="tob_basis_test_")
        root = Path(cls.tmp.name)
        cls.runner = BasisRunner(TOB_BIN, root / "scratch", root / "cache")
        import time

        t0 = time.time()
        cls.bases = cls.runner.get_bases(
            cls.sid, cls.raw, [(cls.lat, cls.lon), (cls.lat + 0.5, cls.lon)]
        )
        cls.run_seconds = time.time() - t0

    @classmethod
    def tearDownClass(cls):
        cls.tmp.cleanup()

    def test_24hr_is_zero(self):
        for basis in self.bases:
            offs = basis.code_offsets_by_ym["24HR"]
            self.assertTrue(offs, "no offsets for 24HR")
            self.assertEqual(set(offs.values()), {0})

    def test_07hr_seasonal_and_near_periodic(self):
        basis = self.bases[0]
        offs = basis.code_offsets_by_ym["07HR"]
        self.assertTrue(
            any(v != 0 for v in offs.values()), "07HR should adjust a CONUS station"
        )
        # Value-dependent rounding: year-varying offsets are EXPECTED at
        # half-cent knife edges, but variation must be confined to +/-1
        # around the majority and recorded in anomalies.
        anom_months = {m for (c, m, _d) in basis.anomalies if c == "07HR"}
        per_month = {}
        for (y, m), off in offs.items():
            per_month.setdefault(m, set()).add(off)
        for m, vals in per_month.items():
            if len(vals) > 1:
                self.assertIn(m, anom_months)
                majority = basis.code_offsets["07HR"][m]
                for v in vals:
                    self.assertLessEqual(
                        abs(v - majority),
                        1,
                        f"knife-edge variation beyond +/-1 in month {m}",
                    )

    def test_solve_a_eff_fraction_recovery(self):
        basis = self.bases[0]
        raw_values = read_station_values(self.raw)
        widths = {}
        for code in ("07HR", "17HR"):
            for month in range(1, 13):
                n_pairs = sum(
                    1
                    for (y, m) in basis.code_offsets_by_ym[code]
                    if m == month and (y, m) in raw_values
                )
                if n_pairs < 20:
                    continue
                iv = basis.solve_a_eff(code, month, raw_values)
                self.assertIsNotNone(iv, f"{code} m{month}: empty interval")
                width = iv.hi - iv.lo
                widths[(code, month)] = (width, n_pairs)
                self.assertLess(
                    width, 0.01, f"{code} m{month}: width {width} not sub-cent"
                )
                # forward check: the midpoint reproduces every observed t
                mid = fp32.f32((iv.lo + iv.hi) / 2.0)
                for (y, m), off in basis.code_offsets_by_ym[code].items():
                    if m != month or (y, m) not in raw_values:
                        continue
                    v = raw_values[(y, m)]
                    self.assertEqual(
                        fp32.pha_qcf(v, mid),
                        v + off,
                        f"{code} m{month} y{y}: forward check failed",
                    )
        self.assertTrue(widths, "no (code, month) had enough pairs")
        # Report the measurement (visible with -v / on failure investigations)
        wmax = max(w for (w, _n) in widths.values())
        wmin = min(w for (w, _n) in widths.values())
        print(
            f"\n[a_eff recovery] {len(widths)} cells, "
            f"width min={wmin:.3e} max={wmax:.3e} degC"
        )

    def test_coords_give_independent_tables(self):
        a, b = self.bases
        self.assertNotEqual(a.code_offsets, b.code_offsets)

    def test_real_station_marked_adjusted(self):
        for basis in self.bases:
            self.assertTrue(basis.station_adjusted)

    def test_cache_hit(self):
        again = self.runner.get_bases(
            self.sid, self.raw, [(self.lat, self.lon), (self.lat + 0.5, self.lon)]
        )
        self.assertEqual(again[0].code_offsets, self.bases[0].code_offsets)

    def test_blend(self):
        basis = self.bases[0]
        years = sorted({y for (y, m) in basis.code_offsets_by_ym["07HR"]})
        # a mid-record year with data in June for a stable blend check
        year = None
        for y in years[len(years) // 2 :]:
            if (y, 6) in basis.code_offsets_by_ym["07HR"]:
                year = y
                break
        self.assertIsNotNone(year)
        blends = self.runner.blend_offsets(
            self.sid,
            self.raw,
            (self.lat, self.lon),
            (year, 6),
            "07HR",
            "17HR",
            [5, 15, 25],
        )
        left = basis.code_offsets_by_ym["07HR"]
        right = basis.code_offsets_by_ym["17HR"]
        for day, offs in blends.items():
            for (y, m), off in offs.items():
                if y < year or (y == year and m < 6):
                    self.assertEqual(
                        off, left[(y, m)], f"pre-switch {(y, m)} day {day}"
                    )
                elif y > year or (y == year and m > 6):
                    self.assertEqual(
                        off, right[(y, m)], f"post-switch {(y, m)} day {day}"
                    )
                else:
                    lo = min(left[(y, m)], right[(y, m)])
                    hi = max(left[(y, m)], right[(y, m)])
                    self.assertTrue(
                        lo <= off <= hi,
                        f"blend {off} outside [{lo},{hi}] " f"day {day}",
                    )

    def test_runtime_budget(self):
        self.assertLess(
            self.run_seconds, 120, f"basis run took {self.run_seconds:.1f}s"
        )


if __name__ == "__main__":
    unittest.main()
