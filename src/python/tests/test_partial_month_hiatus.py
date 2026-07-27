"""Hiatus-adjacency partial-month evidence rule: a single deviant month
immediately before or after a zero-data gap of at least HIATUS_MIN_MONTHS
in the raw record is relabeled with evidence; months deeper in the record,
or gaps shorter than the threshold, are untouched."""

import sys
import types
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from reconstruct_his import classify_partial_months  # noqa: E402


def _vals(months):
    return {ym: 100 for ym in months}


class TestHiatusRule(unittest.TestCase):
    def _run(self, raw_months, deviants):
        raw = _vals(raw_months)
        qcf = dict(raw)
        shim = types.SimpleNamespace(deviants=deviants)
        classify_partial_months(shim, raw, qcf, [], None)
        return shim.deviants

    def test_trailing_month_before_long_gap(self):
        raw_months = (
            [(1980, m) for m in range(1, 13)]
            + [(1991, 7)]
            + [(2002, m) for m in range(1, 13)]
        )
        out = self._run(raw_months, [((1991, 7), "unexplained")])
        self.assertTrue(out[0][1].startswith("partial-month-evidence:hiatus-"))

    def test_leading_month_after_long_gap(self):
        raw_months = [(1980, 1)] + [(1990, m) for m in range(6, 13)]
        out = self._run(raw_months, [((1990, 6), "blend-unresolved")])
        self.assertTrue(out[0][1].startswith("partial-month-evidence:hiatus-"))

    def test_short_gap_not_evidence(self):
        # 12-month gap: below HIATUS_MIN_MONTHS (18)
        raw_months = [(1980, 1)] + [(1981, m) for m in range(2, 13)]
        out = self._run(raw_months, [((1981, 2), "unexplained")])
        self.assertEqual(out[0][1], "unexplained")

    def test_interior_month_not_evidence(self):
        raw_months = [(1980, m) for m in range(1, 13)] + [
            (1995, m) for m in range(1, 13)
        ]
        out = self._run(raw_months, [((1995, 6), "unexplained")])
        self.assertEqual(out[0][1], "unexplained")

    def test_qc_flagged_month_breaks_hiatus(self):
        # data present mid-gap (even if QC-flagged in reality, it appears in
        # raw values) => gap is split below threshold on both sides
        # (gaps of 15 and 16 months, each < HIATUS_MIN_MONTHS)
        raw_months = [(1980, 1), (1981, 5), (1982, 10)]
        out = self._run(raw_months, [((1980, 1), "unexplained")])
        self.assertEqual(out[0][1], "unexplained")


if __name__ == "__main__":
    unittest.main()
