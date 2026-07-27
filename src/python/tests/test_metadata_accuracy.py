"""Synthetic tests for metadata_accuracy: PHR fixture lines + fabricated
solutions exercising every classification."""

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import ghcn_io  # noqa: E402
import metadata_accuracy as ma  # noqa: E402


def phr_line(sid, begin, end, tobs, elem="TEMP", prog="COOP SOD"):
    """Fixed-width PHR line with fields at the documented offsets."""
    buf = [" "] * 210

    def put(s, lo):
        for k, ch in enumerate(s):
            buf[lo + k] = ch

    put(sid, 85)
    put(begin, 106)
    put(end, 115)
    put(elem, 124)
    put(prog, 135)
    put(tobs, 197)
    return "".join(buf)


def parse_lines(lines, sid):
    recs = []
    for line in lines:
        b = ghcn_io._parse_yyyymmdd(line[106:114])
        e = ghcn_io._parse_yyyymmdd(line[115:123])
        raw = line[197:205].strip()
        obs = None if (not raw or raw in ma.SENTINELS) else raw
        recs.append(ghcn_io.PhrRec(sid, b, e, obs))
    return recs


class TestNormalize(unittest.TestCase):
    def test_hours_specials_sentinels(self):
        self.assertEqual(ma.normalize_obs("0800"), (frozenset({"08HR"}), "hour"))
        self.assertEqual(ma.normalize_obs("2400"), (frozenset({"24HR"}), "hour"))
        self.assertEqual(ma.normalize_obs("0000"), (frozenset({"24HR"}), "hour"))
        self.assertEqual(
            ma.normalize_obs("0730"), (frozenset({"07HR", "08HR"}), "half-hour")
        )
        self.assertEqual(
            ma.normalize_obs("0030"), (frozenset({"24HR", "01HR"}), "half-hour")
        )
        self.assertEqual(ma.normalize_obs("SS"), (frozenset({"00SS"}), "special"))
        for bad in ("9999", "UNKN", "VAR", "", None):
            self.assertEqual(ma.normalize_obs(bad)[0], None)
        self.assertEqual(ma.normalize_obs("2799")[1], "unmappable")
        self.assertEqual(ma.normalize_obs("ABCD")[1], "unmappable")


def _sol(regimes, exact=True):
    return {
        "kind": "tob",
        "exact": exact,
        "regimes": [
            {"begin": list(b), "code": c, "blend_day": bd} for b, c, bd in regimes
        ],
    }


def _months(y0, y1, skip=()):
    out = set()
    for y in range(y0, y1 + 1):
        for m in range(1, 13):
            if (y, m) in skip:
                continue
            out.add(y * 12 + m - 1)
    return out


class TestScoring(unittest.TestCase):
    SID = "USX00000001"

    def _recs(self, periods):
        return [ghcn_io.PhrRec(self.SID, b, e, t) for b, e, t in periods]

    def test_full_classification_matrix(self):
        # meta: 17HR from 1950; changes:
        #  (1960,6,15)->0700  [recovered same code, day-resolved 15th]  code_day
        #  (1970,3,1)->0800   [recovered 08HR at (1970,3,20)]          code_month (day off)
        #  (1980,5,1)->1600   [recovered 16HR at (1980,7,1)]           code_near
        #  (1990,2,1)->1800   [recovered 17HR at (1990,2,1)]           time_only_day + confusion
        #  (2000,9,1)->0900   [nothing recovered nearby]               missed
        recs = self._recs(
            [
                ((1950, 1, 1), (1960, 6, 14), "1700"),
                ((1960, 6, 15), (1970, 2, 28), "0700"),
                ((1970, 3, 1), (1980, 4, 30), "0800"),
                ((1980, 5, 1), (1990, 1, 31), "1600"),
                ((1990, 2, 1), (2000, 8, 31), "1800"),
                ((2000, 9, 1), None, "0900"),
            ]
        )
        sol = _sol(
            [
                ((1950, 1, 1), "17HR", None),
                ((1960, 6, 15), "07HR", 15),
                ((1970, 3, 20), "08HR", 20),
                ((1980, 7, 1), "16HR", None),
                ((1990, 2, 1), "17HR", None),
                ((2010, 1, 1), "07HR", None),  # undocumented change
            ]
        )
        sc, _ = ma.score_station(self.SID, sol, recs, _months(1950, 2012))
        self.assertEqual(sc.n_meta, 5)
        self.assertEqual(sc.n_verifiable, 5)
        self.assertEqual(sc.code_day, 1)
        self.assertEqual(sc.code_month, 1)
        self.assertEqual(sc.code_near, 1)
        self.assertEqual(sc.time_only_day, 1)
        self.assertEqual(sc.missed, 1)
        self.assertEqual(sc.undocumented, 1)
        self.assertEqual(sc.confusion[("18HR", "17HR")], 1)

    def test_unverifiable_gap_and_edges(self):
        skip = {(1970, m) for m in range(1, 13)}  # 12-month hole
        months = _months(1950, 1990, skip=skip)
        recs = self._recs(
            [
                (
                    (1940, 1, 1),
                    (1969, 12, 31),
                    "1700",
                ),  # before record: event? first period no event
                (
                    (1970, 5, 1),
                    (1985, 12, 31),
                    "0700",
                ),  # change inside the hole -> unverifiable
                ((1995, 1, 1), None, "0800"),  # after record end -> unverifiable
            ]
        )
        sol = _sol([((1950, 1, 1), "17HR", None), ((1971, 1, 1), "07HR", None)])
        sc, _ = ma.score_station(self.SID, sol, recs, months)
        self.assertEqual(sc.n_meta, 2)
        self.assertEqual(sc.unverifiable, 2)
        self.assertEqual(sc.n_verifiable, 0)
        # the recovered event goes undocumented (its meta twin is unverifiable)
        self.assertEqual(sc.undocumented, 1)

    def test_no_tob_solution_unverifiable(self):
        recs = self._recs(
            [
                ((1950, 1, 1), (1960, 1, 1), "1700"),
                ((1960, 1, 2), None, "0700"),
            ]
        )
        sc, _ = ma.score_station(
            self.SID, {"kind": "pha-only", "regimes": []}, recs, _months(1950, 1990)
        )
        self.assertEqual(sc.unverifiable, 1)
        self.assertEqual(sc.n_verifiable, 0)

    def test_unknown_period_boundary_flagged(self):
        recs = self._recs(
            [
                ((1950, 1, 1), (1959, 12, 31), "1700"),
                ((1960, 1, 1), (1964, 12, 31), "UNKN"),
                ((1965, 1, 1), None, "0700"),
            ]
        )
        events, _ = ma.meta_events(recs)
        self.assertEqual(len(events), 1)
        self.assertTrue(events[0].uncertain_timing)
        sol = _sol([((1950, 1, 1), "17HR", None), ((1965, 1, 1), "07HR", None)])
        sc, _ = ma.score_station(self.SID, sol, recs, _months(1950, 1990))
        self.assertEqual(sc.unverifiable, 1)  # unknown-period-adjacent

    def test_half_hour_matches_either_side(self):
        recs = self._recs(
            [
                ((1950, 1, 1), (1960, 5, 31), "1700"),
                ((1960, 6, 1), None, "0730"),
            ]
        )
        sol = _sol([((1950, 1, 1), "17HR", None), ((1960, 6, 1), "08HR", None)])
        sc, _ = ma.score_station(self.SID, sol, recs, _months(1950, 1990))
        self.assertEqual(sc.code_day, 1)

    def test_phr_line_roundtrip(self):
        line = phr_line("USX00000001", "19600615", "19700228", "0700")
        recs = parse_lines([line], "USX00000001")
        self.assertEqual(recs[0].begin, (1960, 6, 15))
        self.assertEqual(recs[0].end, (1970, 2, 28))
        self.assertEqual(recs[0].obs_time, "0700")
        codes, kind = ma.normalize_obs(recs[0].obs_time)
        self.assertEqual((codes, kind), (frozenset({"07HR"}), "hour"))


if __name__ == "__main__":
    unittest.main()
