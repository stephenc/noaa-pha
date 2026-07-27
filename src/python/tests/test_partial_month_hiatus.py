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


class TestQcfConstraintGap(unittest.TestCase):
    """A QCF-constraint gap (QCU present, QCF absent >= threshold) exempts the
    restart month, even though the RAW record is continuous."""

    def test_qcf_gap_restart_exempt(self):
        from reconstruct_his import _edge_positions

        # QCU continuous 1980-1990; QCF absent 1985-1986 (24-month gap).
        raw = _vals([(y, m) for y in range(1980, 1991) for m in range(1, 13)])
        qcf = _vals(
            [(y, m) for y in range(1980, 1985) for m in range(1, 13)]
            + [(y, m) for y in range(1987, 1991) for m in range(1, 13)]
        )
        pos = _edge_positions(raw, qcf, None)
        mi_restart = 1987 * 12 + 0  # 1987-01
        mi_last = 1984 * 12 + 11  # 1984-12
        self.assertIn(mi_restart, pos)
        self.assertTrue(pos[mi_restart].startswith("qcf-gap-"))
        self.assertTrue(pos[mi_last].startswith("qcf-gap-"))
        # A raw hiatus would still win the label where both apply.
        raw2 = _vals(
            [(y, m) for y in range(1980, 1985) for m in range(1, 13)]
            + [(y, m) for y in range(1987, 1991) for m in range(1, 13)]
        )
        pos2 = _edge_positions(raw2, raw2, None)
        self.assertTrue(pos2[mi_restart].startswith("hiatus-"))

    def test_classify_relabels_qcf_gap(self):
        raw = _vals([(y, m) for y in range(1980, 1991) for m in range(1, 13)])
        qcf = _vals(
            [(y, m) for y in range(1980, 1985) for m in range(1, 13)]
            + [(y, m) for y in range(1987, 1991) for m in range(1, 13)]
        )
        shim = types.SimpleNamespace(deviants=[((1987, 1), "unexplained")])
        classify_partial_months(shim, raw, qcf, [], None)
        self.assertTrue(shim.deviants[0][1].startswith("partial-month-evidence:qcf-gap-"))


if __name__ == "__main__":
    unittest.main()
