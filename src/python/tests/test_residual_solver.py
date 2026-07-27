"""Synthetic forward-model tests for residual_solver, plus guarded real-data
integration.

Synthetic stations are generated THROUGH the exact forward model
(fp32.pha_qcf), so every assertion is against ground truth the solver must
recover bit-exactly.
"""

import random
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import fp32  # noqa: E402
import residual_solver as rs  # noqa: E402
from tob_basis import Basis  # noqa: E402

REPO = Path(__file__).resolve().parents[3]
RAW_DIR = REPO / "data" / "input" / "raw" / "tavg"
QCF_DIR = REPO / "data" / "output" / "qcf" / "tavg"
INV_PATH = REPO / "data" / "input" / "station.inv"
TOB_BIN = REPO / "bin" / "TOBMain"

HAVE_DATA = RAW_DIR.is_dir() and QCF_DIR.is_dir() and INV_PATH.exists()


def month_range(y0, m0, y1, m1):
    """Inclusive (y, m) range."""
    out = []
    y, m = y0, m0
    while (y, m) <= (y1, m1):
        out.append((y, m))
        m += 1
        if m == 13:
            y, m = y + 1, 1
    return out


def synth(seed, yms, tobo_fn, s_fn):
    """Generate (qcu, qcf) through the exact forward model.

    tobo_fn(y, m) -> integer TOB offset (tob - raw); s_fn(y, m) -> float32 S.
    """
    rng = random.Random(seed)
    qcu, qcf = {}, {}
    for y, m in yms:
        v = rng.randint(-300, 3200)
        qcu[(y, m)] = v
        t = v + tobo_fn(y, m)
        qcf[(y, m)] = fp32.pha_qcf(t, s_fn(y, m))
    return qcu, qcf


def make_basis(yms, code_month_offsets, coord=(40.0, -90.0), sid="USQSYNTH000"):
    """Basis whose by_ym offsets replicate per-month vectors over yms."""
    b = Basis(station_id=sid, coord=coord)
    for code, per_month in code_month_offsets.items():
        b.code_offsets[code] = dict(per_month)
        b.code_offsets_by_ym[code] = {
            (y, m): per_month[m] for (y, m) in yms if m in per_month
        }
    b.station_adjusted = True
    return b


# Two seasonal shapes (never flat -- TOB is never a flat shift).
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
OFF_24 = {m: 0 for m in range(1, 13)}


class TestPhaOnly(unittest.TestCase):
    def test_two_segments_exact(self):
        yms = month_range(1950, 1, 2009, 12)
        s_old = fp32.f32(-0.42)

        def s_fn(y, m):
            return s_old if (y, m) < (1980, 1) else 0.0

        qcu, qcf = synth(1, yms, lambda y, m: 0, s_fn)
        sol = rs.solve_pha_only(qcu, qcf, sid="SYN1")
        self.assertTrue(sol.exact)
        self.assertEqual(len(sol.deviants), 0)
        self.assertEqual(len(sol.segments), 2)
        self.assertEqual(tuple(sol.segments[1].begin), (1980, 1))
        self.assertEqual(tuple(sol.segments[0].end), (1979, 12))
        self.assertTrue(sol.segments[0].s_lo <= s_old <= sol.segments[0].s_hi)
        self.assertTrue(sol.segments[1].s_lo <= 0.0 <= sol.segments[1].s_hi)
        self.assertEqual(sol.stats["seeded"], True)

    def test_trivial_zero_residual(self):
        yms = month_range(1960, 1, 1999, 12)
        qcu, qcf = synth(2, yms, lambda y, m: 0, lambda y, m: 0.0)
        sol = rs.solve_pha_only(qcu, qcf, sid="SYN0")
        self.assertTrue(sol.exact)
        self.assertEqual(len(sol.segments), 1)
        self.assertTrue(sol.segments[0].s_lo <= 0.0 <= sol.segments[0].s_hi)

    def test_vintage_skew_month_flagged(self):
        yms = month_range(1950, 1, 1999, 12)
        qcu, qcf = synth(3, yms, lambda y, m: 0, lambda y, m: 0.0)
        qcf[(1970, 6)] += 3  # QCU revised after NOAA's run, say
        sol = rs.solve_pha_only(qcu, qcf, sid="SYNSKEW")
        self.assertEqual(len(sol.segments), 1)
        self.assertEqual([tuple(ym) for ym, _ in sol.deviants], [(1970, 6)])
        self.assertFalse(sol.exact)  # exact-modulo-deviants is the caller's call

    def test_min_length_dead_end_dp(self):
        # S1 = 0.005 sits on the NINT knife edge: roughly half the months of
        # the older segment are also consistent with S = 0 (q == t), so the
        # greedy backward walk overshoots the true boundary; the DP must
        # place a boundary such that no soft-short segment remains.
        yms = month_range(2000, 1, 2004, 2)  # 50 months
        s_old = fp32.f32(0.005)

        def s_fn(y, m):
            return s_old if (y, m) < (2002, 3) else 0.0  # 27 old / 23 new

        qcu, qcf = synth(4, yms, lambda y, m: 0, s_fn)
        sol = rs.solve_pha_only(qcu, qcf, sid="SYNDP")
        self.assertTrue(sol.exact)
        self.assertEqual(len(sol.segments), 2)
        for seg in sol.segments:
            self.assertGreaterEqual(seg.visible, rs.MIN_SEG_VISIBLE)
        self.assertFalse(any(a.startswith("short-segment") for a in sol.audits))

    def test_tail_deletion_fallback(self):
        yms = month_range(1950, 1, 1999, 12)
        s_old = fp32.f32(-0.3)

        def s_fn(y, m):
            return s_old if (y, m) < (1998, 1) else 0.0

        qcu, qcf = synth(5, yms, lambda y, m: 0, s_fn)
        for ym in month_range(1998, 1, 1999, 12):  # PHA g-flagged the tail era
            del qcf[ym]
        sol = rs.solve_pha_only(qcu, qcf, sid="SYNTAIL")
        self.assertTrue(sol.exact)
        self.assertEqual(len(sol.segments), 1)
        self.assertIn("tail-anchor-fallback", sol.audits)
        self.assertTrue(sol.segments[0].s_lo <= s_old <= sol.segments[0].s_hi)


class TestQcfSubsetInvariant(unittest.TestCase):
    """QCF is derived from QCU, so its date set must be a subset of QCU's.
    A month in QCF but not QCU is physically impossible and almost always a
    swapped qcu/qcf pair -- prepare_series must fail loud, not carry phantom
    'qcf-only' deviants downstream."""

    def test_qcf_only_month_raises(self):
        yms = month_range(1990, 1, 1992, 12)
        qcu, qcf = synth(7, yms, lambda y, m: OFF_17[m], lambda y, m: 0.0)
        # A month present in QCF but absent from QCU cannot happen for real.
        qcf[(1993, 1)] = 1234
        with self.assertRaises(ValueError) as ctx:
            rs.prepare_series("USQSWAP0001", qcu, qcf)
        self.assertIn("1993-01", str(ctx.exception))

    def test_swapped_pair_raises(self):
        # Swapping the args is the common operator error this guards against:
        # qcf carries TOB+PHA rounding so it is rarely a subset of qcu.
        yms = month_range(1990, 1, 1995, 12)
        qcu, qcf = synth(9, yms, lambda y, m: OFF_07[m], lambda y, m: 0.3)
        # Only meaningful if the swap actually introduces a qcf-not-in-qcu
        # month; identical key sets would not (values differ, keys match), so
        # perturb one key on the (wrongly-passed) qcu side.
        qcu[(1996, 1)] = 5  # now qcf(=orig qcu) has a month qcu(=orig qcf) lacks
        with self.assertRaises(ValueError):
            rs.prepare_series("USQSWAP0002", qcf, qcu)

    def test_qcf_subset_ok(self):
        # Legitimate case: QCF a strict date-subset of QCU (later months
        # dropped by QC) must solve without error and yield no qcf-only state.
        yms = month_range(1990, 1, 1994, 12)
        qcu, qcf = synth(11, yms, lambda y, m: OFF_17[m], lambda y, m: 0.0)
        del qcf[(1994, 12)]
        series = rs.prepare_series("USQSUBSET01", qcu, qcf)
        self.assertEqual(series.qcf_only, [])
        self.assertNotIn(rs._mi(1994, 12), series.months)


class TestDriverGate(unittest.TestCase):
    """Fix 1: a pure-TOB station must not slip through the PHA-only fast
    path by fragmenting into interval-feasible mini-segments."""

    QUARTERLY = {
        1: 0, 2: 0, 3: 0, 4: 5, 5: 5, 6: 5,
        7: 10, 8: 10, 9: 10, 10: 5, 11: 5, 12: 5,
    }  # fmt: skip

    def test_pure_tob_not_pha_fast_path(self):
        import reconstruct_his as driver

        yms = month_range(1950, 1, 2009, 12)
        qcu, qcf = synth(20, yms, lambda y, m: self.QUARTERLY[m], lambda y, m: 0.0)
        pha_sol = rs.solve_pha_only(qcu, qcf, sid="SYNQTR")
        # fragmenting reading: nominally exact, structurally absurd
        self.assertGreater(len(pha_sol.segments), 100)
        self.assertFalse(driver.pha_fast_path_ok(pha_sol))
        basis = make_basis(
            yms, {"17HR": OFF_17, "QTHR": self.QUARTERLY, "24HR": OFF_24}
        )
        tob_sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNQTR")
        self.assertTrue(tob_sol.exact)
        self.assertEqual([r.code for r in tob_sol.regimes], ["QTHR"])
        self.assertEqual(len(tob_sol.segments), 1)
        self.assertTrue(driver.prefer_tob(pha_sol, tob_sol))


class TestRepairDistribute(unittest.TestCase):
    """Fix 2: a vintage-skew month adjacent to a true changepoint must not
    drag a genuine neighbour month into a wrong mini-segment."""

    def test_skew_adjacent_to_changepoint(self):
        yms = month_range(1950, 1, 2009, 12)
        s_old = fp32.f32(0.15)

        def s_fn(y, m):
            return s_old if (y, m) < (1980, 1) else 0.0

        qcu, qcf = synth(21, yms, lambda y, m: 0, s_fn)
        qcu[(1980, 1)] += 14  # QCU revised after NOAA's run
        sol = rs.solve_pha_only(qcu, qcf, sid="SYNADJ")
        self.assertEqual(len(sol.segments), 2)
        self.assertEqual([tuple(ym) for ym, _ in sol.deviants], [(1980, 1)])
        self.assertEqual(tuple(sol.segments[0].end), (1979, 12))
        self.assertEqual(tuple(sol.segments[1].begin), (1980, 2))
        self.assertTrue(sol.segments[0].s_lo <= s_old <= sol.segments[0].s_hi)
        self.assertTrue(sol.segments[1].s_lo <= 0.0 <= sol.segments[1].s_hi)


def _step_s_fn(breaks):
    """breaks: list of ((y, m), S) ascending; S applies FROM that month."""

    def s_fn(y, m):
        s = 0.0
        for (by, bm), val in breaks:
            if (y, m) >= (by, bm):
                s = val
        return s

    return s_fn


class TestManyChangepoints(unittest.TestCase):
    """Shapes of the three real search-failure stations."""

    def test_bracketed_tob_seven_changepoints(self):
        # USC00487105 shape: starts AND ends without TOB adjustment, TOB
        # regimes in the middle, ~7 PHA level shifts.
        yms = month_range(1900, 1, 2019, 12)
        s_vals = [0.31, -0.22, 0.44, 0.11, -0.35, 0.27, 0.0]
        breaks = [((1915 + 15 * k, 1), fp32.f32(v)) for k, v in enumerate(s_vals)]
        s_fn = _step_s_fn([((1900, 1), fp32.f32(0.18))] + breaks)

        def tobo_fn(y, m):
            if (y, m) < (1907, 1) or (y, m) >= (2016, 3):
                return 0  # no-TOB eras bracket the record
            if (y, m) < (1950, 1):
                return OFF_17[m]
            return OFF_07[m]

        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        qcu, qcf = synth(40, yms, tobo_fn, s_fn)
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNBRACKET")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        codes = [r.code for r in sol.regimes]
        self.assertEqual(codes[0], "24HR")
        self.assertEqual(codes[-1], "24HR")
        self.assertEqual(codes, ["24HR", "17HR", "07HR", "24HR"])
        self.assertEqual(tuple(sol.regimes[1].begin), (1907, 1, 1))
        self.assertEqual(tuple(sol.regimes[3].begin), (2016, 3, 1))
        self.assertEqual(len(sol.segments), 8)

    def test_single_code_nine_changepoints(self):
        # USC00410611/USC00024977 shape: TOB throughout (or none) with many
        # PHA level shifts.
        yms = month_range(1917, 1, 2021, 12)
        s_vals = [0.45, -0.31, 0.22, -0.15, 0.38, -0.42, 0.19, -0.27, 0.0]
        breaks = [((1930 + 10 * k, 6), fp32.f32(v)) for k, v in enumerate(s_vals)]
        s_fn = _step_s_fn([((1917, 1), fp32.f32(-0.2))] + breaks)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        qcu, qcf = synth(41, yms, lambda y, m: OFF_17[m], s_fn)
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNNINE")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        self.assertEqual([r.code for r in sol.regimes], ["17HR"])
        self.assertEqual(len(sol.segments), 10)
        self.assertLess(sol.stats.get("pops", 10**9), rs.SEARCH_POP_CAP)


OFF_15 = {
    1: -9,
    2: -8,
    3: -9,
    4: -8,
    5: -6,
    6: -5,
    7: -4,
    8: -5,
    9: -6,
    10: -7,
    11: -7,
    12: -8,
}


class TestFrontierCapAudit(unittest.TestCase):
    """The PARETO_CAP frontier eviction is a lexicographic heuristic that can
    drop the optimal path.  It must not do so silently: any search that hits
    the cap sets `capped`, surfaced downstream as a `frontier-capped` audit."""

    def _bracket_inputs(self):
        yms = month_range(1900, 1, 2019, 12)
        s_vals = [0.31, -0.22, 0.44, 0.11, -0.35, 0.27, 0.0]
        breaks = [((1915 + 15 * k, 1), fp32.f32(v)) for k, v in enumerate(s_vals)]
        s_fn = _step_s_fn([((1900, 1), fp32.f32(0.18))] + breaks)

        def tobo_fn(y, m):
            if (y, m) < (1907, 1) or (y, m) >= (2016, 3):
                return 0
            if (y, m) < (1950, 1):
                return OFF_17[m]
            return OFF_07[m]

        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        qcu, qcf = synth(40, yms, tobo_fn, s_fn)
        return qcu, qcf, basis

    def test_cap_sets_capped_flag(self):
        from unittest import mock

        qcu, qcf, basis = self._bracket_inputs()
        series = rs.prepare_series("SYNCAP", qcu, qcf)
        # A cap of 1 forces frontier pressure on this 8-changepoint shape.
        with mock.patch.object(rs, "PARETO_CAP", 1):
            res = rs._solve_with_basis(series, basis, True)
        self.assertIsNotNone(res)
        self.assertTrue(res["capped"])

    def test_normal_solve_not_capped(self):
        # Under the default cap this shape solves exactly with no frontier
        # pressure, so the audit must be absent -- the marker means something.
        qcu, qcf, basis = self._bracket_inputs()
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNCAP")
        self.assertTrue(sol.exact)
        self.assertNotIn("frontier-capped", sol.audits)


class TestDeviantClusters(unittest.TestCase):
    """Shapes of four real multi-month deviant clusters: gap-edge short
    segments, trial regimes at unchanged S, era continuation over a fake
    regime, and a record-edge isolated month."""

    def test_gap_edge_short_segment(self):
        # USC00051772/USC00474829 shape: a changepoint leaves only 2 visible
        # months before a long data hole (X-flagged in the real cases -- PHA
        # counted them as process months); gap-aware hard-short must allow it.
        yms = [
            ym
            for ym in month_range(1950, 1, 2009, 12)
            if not ((2000, 7) <= ym <= (2002, 6))
        ]
        s_mid = fp32.f32(-1.98)

        def s_fn(y, m):
            if (y, m) < (2000, 5):
                return fp32.f32(-0.63)
            if (y, m) <= (2000, 6):
                return s_mid
            return 0.0

        qcu, qcf = synth(50, yms, lambda y, m: OFF_17[m], s_fn)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNGAPEDGE")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        self.assertEqual([r.code for r in sol.regimes], ["17HR"])
        self.assertEqual(len(sol.segments), 3)
        mid = sol.segments[1]
        self.assertEqual(mid.n_constraints, 2)
        self.assertTrue(mid.s_lo <= s_mid <= mid.s_hi)

    def test_trial_regime_same_s(self):
        # USC00049099 shape: a genuine 2-month trial code at the UNCHANGED
        # era S, bracketed by different codes; needs componentwise dominance
        # and the no-blend rerun to be found.
        yms = month_range(1950, 1, 2009, 12)
        s_era = fp32.f32(-0.30)

        def tobo_fn(y, m):
            if (y, m) < (1985, 11):
                return OFF_17[m]
            if (y, m) <= (1985, 12):
                return OFF_07[m]
            return OFF_15[m]

        qcu, qcf = synth(51, yms, tobo_fn, lambda y, m: s_era)
        basis = make_basis(
            yms, {"17HR": OFF_17, "07HR": OFF_07, "15HR": OFF_15, "24HR": OFF_24}
        )
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNTRIAL")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        self.assertEqual([r.code for r in sol.regimes], ["17HR", "07HR", "15HR"])
        self.assertEqual(tuple(sol.regimes[1].begin), (1985, 11, 1))
        self.assertEqual(tuple(sol.regimes[2].begin), (1986, 1, 1))
        self.assertEqual(len(sol.segments), 1)

    def test_continuation_over_fake_regime(self):
        # USC00090581 shape: the old-code era continues right up to the
        # switch; a lexicographic dominance prune used to bury the
        # continuation path under a fabricated intermediate regime.
        yms = [
            ym
            for ym in month_range(1901, 5, 1977, 1)
            if not ((1944, 5) <= ym <= (1944, 12))
        ]

        def tobo_fn(y, m):
            return OFF_17[m] if (y, m) < (1944, 4) else OFF_07[m]

        def s_fn(y, m):
            return fp32.f32(-0.66) if (y, m) < (1944, 4) else 0.0

        qcu, qcf = synth(52, yms, tobo_fn, s_fn)
        basis = make_basis(
            yms, {"17HR": OFF_17, "07HR": OFF_07, "23HR": OFF_15, "24HR": OFF_24}
        )
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNCONT")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        self.assertEqual([r.code for r in sol.regimes], ["17HR", "07HR"])
        self.assertEqual(tuple(sol.regimes[1].begin), (1944, 4, 1))

    def test_record_edge_isolated_month(self):
        # USC00410611 shape: one month decades after the record body, in its
        # own S=0 segment; record-edge exemption lets it be a segment rather
        # than a deviant or a churn of fake regimes.
        yms = month_range(1901, 11, 1978, 4) + [(2003, 9)]
        s_old = fp32.f32(-0.46)

        def s_fn(y, m):
            return s_old if (y, m) <= (1978, 4) else 0.0

        qcu, qcf = synth(53, yms, lambda y, m: OFF_17[m], s_fn)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNEDGE")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        self.assertEqual([r.code for r in sol.regimes], ["17HR"])
        self.assertEqual(len(sol.segments), 2)
        self.assertEqual(sol.segments[1].n_constraints, 1)


class TestFlutterAndVacuousFlips(unittest.TestCase):
    """USC00456678 / USC00034572 / USC00487105-calibration shapes."""

    def _find_flutter_month(self, yms, s_era, tobo_fn, qcu, qcf):
        """Craft one month whose S-interval misses the era's running
        intersection by < FLUTTER_EPS: scan candidate values for a
        sub-1e-5 endpoint gap (float32 endpoints quantize ~1.5e-8)."""
        import residual_solver as rs_mod

        series = rs.prepare_series("SCAN", qcu, qcf)
        run_iv = None
        for i in range(len(series.months)):
            iv = rs._psi(series.t_raw[i], series.q[i])
            run_iv = iv if run_iv is None else rs._isect(run_iv, iv)
        assert run_iv is not None
        for t in range(200, 4200):
            q = fp32.pha_qcf(t, run_iv.lo) - 1  # one cent below the era
            iv = rs._psi(t, q)
            gap = rs._flutter_gap(iv, run_iv)
            if gap is not None and 0.0 < gap < rs.FLUTTER_EPS:
                return t, q
        raise AssertionError("no knife-edge candidate found in scan")

    def test_flutter_month_beats_excursion(self):
        # sub-1e-5 miss -> single regime + one flutter deviant; the solver
        # must NOT invent a code excursion + two changepoints around it.
        yms = month_range(1958, 1, 1966, 6)
        s_era = fp32.f32(0.14)
        qcu, qcf = synth(60, yms, lambda y, m: 0, lambda y, m: s_era)
        t_f, q_f = self._find_flutter_month(yms, s_era, None, qcu, qcf)
        qcu[(1963, 9)] = t_f
        qcf[(1963, 9)] = q_f
        basis = make_basis(yms, {"17HR": OFF_17, "18HR": OFF_07, "24HR": OFF_24})
        # PHA-only view of the same signal (tobo == 0 via 24HR)
        sol = rs.solve_pha_only(qcu, qcf, sid="SYNFLUT")
        self.assertEqual(len(sol.segments), 1)
        self.assertEqual(
            [w for _ym, w in sol.deviants], ["flutter"], msg=str(sol.deviants)
        )
        self.assertTrue(sol.exact)  # flutter does not break exactness
        self.assertFalse(any("s-excursion" in a for a in sol.audits))

    def test_degree_scale_trial_not_flutter(self):
        # calibration: a genuine documented trial misses by DEGREES -- the
        # flutter discriminator is interval-gap scale, never amplitude.
        yms = month_range(1950, 1, 2009, 12)
        s_era = fp32.f32(-0.30)

        def tobo_fn(y, m):
            if (y, m) < (1987, 12):
                return OFF_07[m]
            if (y, m) <= (1988, 1):
                return OFF_15[m]
            return OFF_07[m]

        qcu, qcf = synth(61, yms, tobo_fn, lambda y, m: s_era)
        basis = make_basis(yms, {"07HR": OFF_07, "15HR": OFF_15, "24HR": OFF_24})
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNTRIALKEEP")
        self.assertTrue(sol.exact)
        self.assertEqual([r.code for r in sol.regimes], ["07HR", "15HR", "07HR"])
        self.assertFalse(any(w == "flutter" for _ym, w in sol.deviants))

    def test_vacuous_flip_merged(self):
        # USC00034572 shape: an alternative code with IDENTICAL offsets over
        # the observed span (00RS==00SS Nov-Mar) must never be flipped to.
        yms = [
            ym for ym in month_range(1950, 1, 2009, 12) if ym[1] in (11, 12, 1, 2, 3)
        ]
        # 00RS == 00SS on every observed (winter) month
        OFF_SS = {m: OFF_17[m] for m in range(1, 13)}
        s_fn = _step_s_fn([((1975, 3), fp32.f32(0.125))])
        qcu, qcf = synth(62, yms, lambda y, m: OFF_SS[m], s_fn)
        basis = make_basis(yms, {"00SS": OFF_SS, "00RS": OFF_SS, "24HR": OFF_24})
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNVACUOUS")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:4]}")
        self.assertEqual(len(sol.regimes), 1, msg=str([r.code for r in sol.regimes]))
        self.assertEqual(len(sol.segments), 2)


class TestTwoBlendRepair(unittest.TestCase):
    """USC00134063 shape: short interior regime with entry AND exit blends."""

    def test_entry_exit_blend_short_regime(self):
        yms = month_range(1980, 1, 1999, 12)
        s_era = fp32.f32(-0.48)
        ENTRY = (1990, 12)
        EXIT = (1991, 2)
        d1, d2 = 4, 9
        o_entry = 31  # day-weighted 24HR->07HR blend at Dec 4
        o_exit = -17  # day-weighted 07HR->18HR blend at Feb 9

        def tobo_fn(y, m):
            if (y, m) < ENTRY:
                return OFF_24[m]
            if (y, m) == ENTRY:
                return o_entry
            if (y, m) < EXIT:
                return OFF_07[m]
            if (y, m) == EXIT:
                return o_exit
            return OFF_15[m]  # stand-in for 18HR

        qcu, qcf = synth(63, yms, tobo_fn, lambda y, m: s_era)
        basis = make_basis(
            yms, {"24HR": OFF_24, "07HR": OFF_07, "15HR": OFF_15, "17HR": OFF_17}
        )

        def blend_fn(coord, ym, c_old, c_new):
            table = {}
            for d in range(2, 29):
                if ym == ENTRY and (c_old, c_new) == ("24HR", "07HR"):
                    table[d] = {ym: o_entry if d == d1 else 999}
                elif ym == EXIT and (c_old, c_new) == ("07HR", "15HR"):
                    table[d] = {ym: o_exit if d == d2 else 999}
                else:
                    table[d] = {ym: 999}
            return table

        sol = rs.solve_tob_station(qcu, qcf, [basis], blend_fn, sid="SYN2BLEND")
        self.assertTrue(
            sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]} aud={sol.audits}"
        )
        codes = [(r.code, r.begin) for r in sol.regimes]
        self.assertIn(("07HR", (1990, 12, d1)), codes, msg=str(codes))
        self.assertIn(("15HR", (1991, 2, d2)), codes, msg=str(codes))
        self.assertTrue(any("two-blend-repair" in a for a in sol.audits))


class TestDriverPolicies(unittest.TestCase):
    def test_sentinel_never_beats_pha(self):
        import reconstruct_his as driver

        pha = rs.Solution(
            "X", "pha-only", None, [], [], [((2000, 1), "repair-merged")], [],
            [], (2, 1, 0, 13, 0, 3), False, {},
        )  # fmt: skip
        sentinel = rs.Solution(
            "X", "tob", None, [], [], [], [], ["no-solution"],
            (10**6, 0, 0, 0, 0, 0), False, {},
        )  # fmt: skip
        self.assertFalse(driver.prefer_tob(pha, sentinel))

    def test_partial_month_classifier(self):
        import ghcn_io
        import reconstruct_his as driver

        sol = rs.Solution(
            "X", "tob", 0, [], [],
            [((2015, 12), "unexplained"), ((1980, 5), "unexplained")],
            [], [], (0, 2, 0, 0, 0, 0), False, {},
        )  # fmt: skip
        raw_vals = {(y, m): 0 for y in range(1950, 2016) for m in range(1, 13)}
        qcf_vals = dict(raw_vals)
        recs = [
            ghcn_io.MshrRec("X", (1950, 1, 1), (2015, 12, 2), 40.0, -90.0, None, "")
        ]
        driver.classify_partial_months(sol, raw_vals, qcf_vals, recs, (2026, 4))
        self.assertEqual(
            sol.deviants[0], ((2015, 12), "partial-month-evidence:20151202")
        )
        # interior deviant untouched even with MSHR present
        self.assertEqual(tuple(sol.deviants[1][0]), (1980, 5))
        self.assertEqual(sol.deviants[1][1], "unexplained")

    def test_partial_month_dataset_last(self):
        import reconstruct_his as driver

        sol = rs.Solution(
            "X", "tob", 0, [], [], [((2026, 4), "unexplained")],
            [], [], (0, 1, 0, 0, 0, 0), False, {},
        )  # fmt: skip
        raw_vals = {(2026, m): 0 for m in range(1, 5)}
        driver.classify_partial_months(sol, raw_vals, dict(raw_vals), None, (2026, 4))
        self.assertEqual(
            sol.deviants[0], ((2026, 4), "partial-month-evidence:dataset-last")
        )


class TestCoordEscalation(unittest.TestCase):
    def _series(self):
        return month_range(1950, 1, 2009, 12)

    def test_mshr_coord_wins_only_by_exactness(self):
        yms = self._series()
        # true offsets = OFF_07; inventory basis disagrees by +2 every March
        # (not +-1, so no knife-edge patch applies); MSHR basis is correct.
        wrong_07 = dict(OFF_07)
        wrong_07[3] = OFF_07[3] + 2
        basis_inv = make_basis(yms, {"07HR": wrong_07, "24HR": OFF_24})
        basis_mshr = make_basis(
            yms, {"07HR": OFF_07, "24HR": OFF_24}, coord=(40.5, -90.5)
        )
        qcu, qcf = synth(30, yms, lambda y, m: OFF_07[m], lambda y, m: 0.0)
        sol = rs.solve_tob_station(
            qcu, qcf, [basis_inv, basis_mshr], None, sid="SYNESC"
        )
        self.assertTrue(sol.exact)
        self.assertEqual(sol.coord_index, 1)

    def test_inventory_kept_when_nothing_exact(self):
        # short series so the search stays cheap; BOTH bases wrong in one
        # month (different deltas) -> nothing exact anywhere; the solver must
        # return the coord-0 (inventory) reading even if coord 1 is cheaper.
        yms = month_range(2000, 1, 2004, 12)
        wrong_inv = dict(OFF_07)
        wrong_inv[3] = OFF_07[3] + 2  # 5 bad Marches at inventory
        wrong_mshr = dict(OFF_07)
        wrong_mshr[3] = OFF_07[3] + 2
        wrong_mshr[7] = OFF_07[7] + 2  # 10 bad months at MSHR: pricier
        basis_inv = make_basis(yms, {"07HR": wrong_inv, "24HR": OFF_24})
        basis_mshr = make_basis(
            yms, {"07HR": wrong_mshr, "24HR": OFF_24}, coord=(40.5, -90.5)
        )
        qcu, qcf = synth(31, yms, lambda y, m: OFF_07[m], lambda y, m: 0.0)
        sol = rs.solve_tob_station(
            qcu, qcf, [basis_inv, basis_mshr], None, sid="SYNESC2"
        )
        # nothing CLEANLY exact anywhere (a fragmented/alternating reading
        # may be nominally exact but carries hard- or soft-shorts) ->
        # inventory kept
        self.assertEqual(sol.coord_index, 0)
        self.assertFalse(rs._clean(sol))


class TestTobStation(unittest.TestCase):
    def _yms(self):
        return month_range(1950, 1, 2009, 12)

    def test_two_regimes_two_changepoints(self):
        yms = self._yms()
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        s1, s2 = fp32.f32(0.61), fp32.f32(-0.27)

        def tobo_fn(y, m):
            return OFF_17[m] if (y, m) < (1975, 6) else OFF_07[m]

        def s_fn(y, m):
            if (y, m) < (1962, 1):
                return s1
            if (y, m) < (1990, 1):
                return s2
            return 0.0

        qcu, qcf = synth(10, yms, tobo_fn, s_fn)
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNTOB2")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        self.assertEqual([r.code for r in sol.regimes], ["17HR", "07HR"])
        self.assertEqual(tuple(sol.regimes[1].begin), (1975, 6, 1))
        self.assertEqual(len(sol.segments), 3)
        self.assertEqual(sol.cost[2], 1)  # one TOB change
        segs = sol.segments
        self.assertTrue(segs[0].s_lo <= s1 <= segs[0].s_hi)
        self.assertTrue(segs[1].s_lo <= s2 <= segs[1].s_hi)
        self.assertTrue(segs[2].s_lo <= 0.0 <= segs[2].s_hi)

    def test_changepoint_and_code_change_same_month(self):
        yms = self._yms()
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        s1 = fp32.f32(0.33)

        def tobo_fn(y, m):
            return OFF_17[m] if (y, m) < (1975, 6) else OFF_07[m]

        def s_fn(y, m):
            return s1 if (y, m) < (1975, 6) else 0.0

        qcu, qcf = synth(11, yms, tobo_fn, s_fn)
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNTOBCC")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        self.assertEqual([r.code for r in sol.regimes], ["17HR", "07HR"])
        self.assertEqual(tuple(sol.regimes[1].begin), (1975, 6, 1))
        self.assertEqual(len(sol.segments), 2)
        self.assertEqual(tuple(sol.segments[1].begin), (1975, 6))

    def test_tob_vintage_skew_deviant(self):
        yms = self._yms()
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})

        def tobo_fn(y, m):
            return OFF_17[m]

        qcu, qcf = synth(12, yms, tobo_fn, lambda y, m: 0.0)
        qcf[(1980, 4)] -= 2
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNTOBSKEW")
        self.assertEqual([tuple(ym) for ym, _ in sol.deviants], [(1980, 4)])
        self.assertEqual([r.code for r in sol.regimes], ["17HR"])
        self.assertEqual(len(sol.segments), 1)

    def test_knife_edge_documented_never_emitted(self):
        yms = self._yms()
        # Solver's basis says July offset is -3; the generator (NOAA, say)
        # actually applied -2 every July -- a half-cent knife edge rounded
        # the other way.  POLICY: the patched reading (which would be fully
        # exact) must NEVER be returned -- the emitted pipeline must
        # self-verify.  The plain reading wins, carrying the cell as a
        # documented compiler-divergence candidate.
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        true_17 = dict(OFF_17)
        true_17[7] = OFF_17[7] + 1

        def tobo_fn(y, m):
            return true_17[m]

        qcu, qcf = synth(13, yms, tobo_fn, lambda y, m: 0.0)
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNKNIFE")
        # plain reading: single 17HR era, every July a deviant
        self.assertEqual([r.code for r in sol.regimes], ["17HR"])
        self.assertFalse(sol.exact)
        dev_months = {ym[1] for ym, _ in sol.deviants}
        self.assertEqual(dev_months, {7})
        # the divergence is documented, not emitted
        self.assertIn(("17HR", 7, 1), [tuple(k) for k in sol.knife_edges])
        self.assertTrue(
            any(
                a.startswith("compiler-divergence-candidate:17HR:m7:+1:")
                for a in sol.audits
            ),
            msg=f"audits={sol.audits}",
        )

    def test_emitted_his_roundtrip(self):
        import ghcn_io
        import his_emit

        yms = self._yms()
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})

        def tobo_fn(y, m):
            return OFF_17[m] if (y, m) < (1975, 6) else OFF_07[m]

        qcu, qcf = synth(14, yms, tobo_fn, lambda y, m: 0.0)
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNEMIT")
        self.assertTrue(sol.exact)
        regimes = [his_emit.Regime(begin=r.begin, obs_time=r.code) for r in sol.regimes]
        inv = ghcn_io.Inv("USQSYNTH000", 40.0, -90.0, 200.0, "SYNTH")
        dms = ghcn_io.dms_quantize(40.0, -90.0)
        with tempfile.TemporaryDirectory() as td:
            path = his_emit.emit_station_his(
                "USQSYNTH000", regimes, dms, inv, Path(td), 1950
            )
            rows = his_emit.validate_his_file(path, first_data_year=1950)
        self.assertEqual([r.obs_time_raw.strip() for r in rows], ["17HR", "07HR"])
        self.assertEqual(rows[0].beg, (1950, 1, 1))
        self.assertEqual(rows[1].beg, (1975, 6, 1))


@unittest.skipUnless(HAVE_DATA, "workspace data not present")
class TestRealData(unittest.TestCase):
    def _load(self, sid):
        import ghcn_io

        raw = ghcn_io.read_station_data(RAW_DIR / f"{sid}.raw.tavg")
        qcf = ghcn_io.read_station_data(QCF_DIR / f"{sid}.qcf.tavg")
        return raw, qcf

    def _noncon_candidates(self, want=3):
        import ghcn_io

        ids = []
        preferred = "ASN00086017"
        if (RAW_DIR / f"{preferred}.raw.tavg").exists() and (
            QCF_DIR / f"{preferred}.qcf.tavg"
        ).exists():
            ids.append(preferred)
        for p in sorted(QCF_DIR.glob("A*.qcf.tavg")):
            sid = p.name.split(".")[0]
            if sid in ids or sid.startswith("US"):
                continue
            raw_p = RAW_DIR / f"{sid}.raw.tavg"
            if not raw_p.exists():
                continue
            raw = ghcn_io.read_station_data(raw_p)
            qcf = ghcn_io.read_station_data(p)
            common = set(raw.values) & set(qcf.values)
            if len(common) < 240:
                continue
            if any(qcf.values[ym] != raw.values[ym] for ym in common):
                ids.append(sid)
            if len(ids) >= want:
                break
        return ids[:want]

    def test_noncon_calibration_sample(self):
        for sid in self._noncon_candidates():
            raw, qcf = self._load(sid)
            sol = rs.solve_pha_only(raw.values, qcf.values, qcf.flags, sid=sid)
            n_dev = len(sol.deviants)
            print(
                f"[noncon] {sid}: exact={sol.exact} segs={len(sol.segments)} "
                f"dev={n_dev} cost={sol.cost}"
            )
            # exact modulo (rare) vintage deviants
            self.assertTrue(
                sol.exact or n_dev <= 3,
                msg=f"{sid}: {n_dev} deviants, cost={sol.cost}",
            )

    @unittest.skipUnless(TOB_BIN.exists(), "TOBMain not built")
    def test_conus_tob_station(self):
        import ghcn_io
        from tob_basis import BasisRunner

        inv_all = ghcn_io.read_inventory(INV_PATH)
        gate_file = REPO / "work" / "conus_tob_gate_ids.txt"
        candidates = []
        if gate_file.exists():
            candidates = [l.strip() for l in open(gate_file) if l.strip()]
        else:
            candidates = [
                sid
                for sid, rec in inv_all.items()
                if sid.startswith("US")
                and 23 <= rec.lat <= 50
                and -126 <= rec.lon <= -65
            ]
        chosen = None
        for sid in candidates[:200]:
            raw_p = RAW_DIR / f"{sid}.raw.tavg"
            qcf_p = QCF_DIR / f"{sid}.qcf.tavg"
            if not (raw_p.exists() and qcf_p.exists()):
                continue
            raw = ghcn_io.read_station_data(raw_p)
            qcf = ghcn_io.read_station_data(qcf_p)
            common = sorted(set(raw.values) & set(qcf.values))
            if len(common) < 480:
                continue
            # want a station whose residual is seasonal (TOB), not flat
            tail = common[-24:]
            resid = {ym: qcf.values[ym] - raw.values[ym] for ym in tail}
            if len(set(resid.values())) >= 3 and any(resid.values()):
                chosen = (sid, raw, qcf)
                break
        self.assertIsNotNone(chosen, "no CONUS TOB candidate found in scan")
        sid, raw, qcf = chosen
        inv = inv_all[sid]
        with tempfile.TemporaryDirectory() as td:
            runner = BasisRunner(TOB_BIN, Path(td), Path(td) / "cache")
            bases = runner.get_bases(
                sid, RAW_DIR / f"{sid}.raw.tavg", [(inv.lat, inv.lon)]
            )
            self.assertTrue(bases[0].station_adjusted)
            sol = rs.solve_tob_station(
                raw.values, qcf.values, bases, None, qcf.flags, sid=sid
            )
        print(
            f"[conus] {sid}: exact={sol.exact} regimes="
            f"{[(r.begin, r.code) for r in sol.regimes]} segs={len(sol.segments)} "
            f"dev={len(sol.deviants)} cost={sol.cost} audits={sol.audits[:4]}"
        )
        self.assertGreaterEqual(len(sol.regimes), 1)
        self.assertEqual(sol.cost[0], 0, "hard-short segments in real solve")
        self.assertLessEqual(len(sol.deviants), 24, "too many deviants")


class TestMagnitudeTieBreak(unittest.TestCase):
    """USC00012727's shape: a regime boundary underdetermined by data (the
    deviant month's successor months are unconstrained), where two equal-cost
    placements differ in unexplained magnitude.  The reading leaving fewer
    unexplained cents must win, and a PHR hint anchors the boundary at the
    documented mid-month date."""

    def test_usc00012727_boundary_refined(self):
        import os

        if not (
            os.path.exists("data/input/raw/tavg/USC00012727.raw.tavg")
            and os.path.exists("bin/TOBMain")
            and os.path.exists("data/phr.txt.zip")
        ):
            self.skipTest("real data not present")
        import json
        import subprocess

        subprocess.run(
            [
                "python3",
                "src/python/reconstruct_his.py",
                "--stations",
                "USC00012727",
                "--mshr-zip",
                "data/mshr_enhanced.txt.zip",
                "--phr-zip",
                "data/phr.txt.zip",
                "--jobs",
                "1",
                "--summary-file",
                "/dev/null",
            ],
            check=True,
            capture_output=True,
        )
        sol = json.load(open("data/intermediate/solutions/USC00012727.json"))
        b1962 = [r for r in sol["regimes"] if r["begin"][0] == 1962]
        self.assertEqual(len(b1962), 1)
        # PHR documents 06HR->07HR at 1962-05-04; June 1962 QCF is deleted,
        # so the boundary is underdetermined -- the magnitude tie-break plus
        # the hint must land it on the documented date.
        self.assertEqual(b1962[0]["begin"], [1962, 5, 4])
        self.assertEqual(b1962[0]["code"], "07HR")
        self.assertIn("magnitude-refined@1962-05-04", sol["audits"])
        self.assertLessEqual(sol["stats"]["deviant_magnitude"], 4)


OFF_16 = {
    1: -8,
    2: -8,
    3: -9,
    4: -8,
    5: -7,
    6: -6,
    7: -5,
    8: -5,
    9: -6,
    10: -7,
    11: -7,
    12: -8,
}


class TestIfortBasisErrorStability(unittest.TestCase):
    """ifort cross-check hardening: a 1-cent single-calendar-month basis
    error must yield the unbroken-era reading (patched or one honest
    deviant), never an invented sub-12-month excursion regime
    (USC00108380/USC00272284/USC00397742 shapes)."""

    def _yms(self):
        return month_range(1950, 1, 2009, 12)

    def _truth(self, seed=11):
        yms = self._yms()
        s_old = fp32.f32(-0.31)
        s_new = fp32.f32(0.12)

        def s_fn(y, m):
            return s_old if (y, m) < (1980, 6) else s_new

        qcu, qcf = synth(seed, yms, lambda y, m: OFF_16[m], s_fn)
        return yms, qcu, qcf

    def _wedge_codes(self, yms, bad_junes, delta=1):
        """Basis: '16HR' with the June cell corrupted (+delta) at the given
        years (value-dependent knife edge: only SOME years flip), plus a
        realistic wedge-enabler '14HR': identical to 16HR in Jun-Aug
        (adjacent-month degeneracy) except June where it supplies the TRUE
        value, and far away (+30) every other month -- the shape that made
        the ifort run's Jun-Aug excursions fit while a whole-record switch
        cannot."""
        off_14 = {m: (OFF_16[m] if m in (6, 7, 8) else OFF_16[m] + 30) for m in OFF_16}
        b = make_basis(yms, {"16HR": OFF_16, "14HR": off_14})
        by16 = b.code_offsets_by_ym["16HR"]
        for ym in bad_junes:
            by16[ym] = by16[ym] + delta  # corrupted cell (14HR June stays true)
        return b

    def test_systematic_bad_month_documented_not_patched(self):
        yms, qcu, qcf = self._truth()
        bad = [(y, 6) for y in range(1955, 2005, 7)]  # 8 poisoned Junes
        basis = self._wedge_codes(yms, bad)
        sol = rs.solve_tob_station(qcu, qcf, [basis], sid="USQIFRT0001")
        codes = {r.code for r in sol.regimes}
        self.assertNotIn("14HR", codes, "excursion regime invented")
        self.assertEqual(codes, {"16HR"})
        # POLICY: the patched reading (fully exact) is diagnostic only; the
        # plain reading is returned -- the poisoned Junes stay as honest
        # flutter deviants (exact-with-flutter, like USC00222099 before its
        # gate row landed) and the cell is documented as a
        # compiler-divergence candidate (delta -1: the patch that FIXES the
        # +1-corrupted basis).  Regimes/segments are the plain reading's.
        dev = sorted((tuple(ym), why) for ym, why in sol.deviants)
        self.assertEqual([d[0] for d in dev], sorted(bad))
        self.assertTrue(all(why == "flutter" for _ym, why in dev), msg=f"{dev}")
        self.assertIn(("16HR", 6, -1), [tuple(k) for k in sol.knife_edges])
        self.assertTrue(
            any(
                a.startswith("compiler-divergence-candidate:16HR:m6:-1:")
                for a in sol.audits
            ),
            msg=f"audits={sol.audits}",
        )

    def test_single_bad_month_prefers_deviant_over_wedge(self):
        yms, qcu, qcf = self._truth()
        bad = [(1963, 6)]  # one poisoned June: below KE_MIN_REPEATS
        basis = self._wedge_codes(yms, bad)
        sol = rs.solve_tob_station(qcu, qcf, [basis], sid="USQIFRT0002")
        codes = {r.code for r in sol.regimes}
        self.assertNotIn("14HR", codes, "suspicious excursion not priced out")
        self.assertEqual(codes, {"16HR"})
        dev_months = [tuple(d[0]) for d in sol.deviants]
        self.assertEqual(dev_months, [(1963, 6)])

    def test_genuine_large_amplitude_trial_survives(self):
        yms = self._yms()
        off_15 = {m: OFF_16[m] + 52 for m in OFF_16}
        s_flat = fp32.f32(-0.31)

        def tobo(y, m):
            if (y, m) in ((1987, 11), (1987, 12)):
                return off_15[m]
            return OFF_16[m]

        qcu, qcf = synth(12, yms, tobo, lambda y, m: s_flat)
        basis = make_basis(yms, {"16HR": OFF_16, "15HR": off_15})
        sol = rs.solve_tob_station(qcu, qcf, [basis], sid="USQIFRT0003")
        self.assertTrue(sol.exact)
        self.assertEqual([d for d in sol.deviants if d[1] != "flutter"], [])
        codes = [r.code for r in sol.regimes]
        self.assertIn("15HR", codes, "genuine 2-month trial suppressed")

    def test_phr_hint_exempts_tiny_excursion(self):
        # Truth GENUINELY contains a Jun-Aug 1980 '14HR' regime whose only
        # offset difference from 16HR is June (+1 cent) -- observationally
        # identical to the basis-error scenario.  Without a hint the stable
        # reading is one deviant; with a PHR hint dating the change, the
        # excursion is exempt from the charge and wins (0 deviants).
        yms = self._yms()
        by14_month = {m: (OFF_16[m] if m in (7, 8) else OFF_16[m] + 30) for m in OFF_16}
        by14_month[6] = OFF_16[6] + 1
        s_flat = fp32.f32(-0.31)

        def tobo(y, m):
            if (1980, 6) <= (y, m) <= (1980, 8):
                return by14_month[m]
            return OFF_16[m]

        qcu, qcf = synth(13, yms, tobo, lambda y, m: s_flat)
        basis = make_basis(yms, {"16HR": OFF_16, "14HR": by14_month})

        no_hint = rs.solve_tob_station(qcu, qcf, [basis], sid="USQIFRT0004")
        self.assertEqual(
            [tuple(d[0]) for d in no_hint.deviants],
            [(1980, 6)],
            "unhinted tiny excursion should resolve to one honest deviant",
        )
        self.assertNotIn("14HR", {r.code for r in no_hint.regimes})

        hinted = rs.solve_tob_station(
            qcu,
            qcf,
            [basis],
            sid="USQIFRT0004",
            hints=[((1980, 6, 1), "14HR"), ((1980, 9, 1), "16HR")],
        )
        self.assertIn("14HR", {r.code for r in hinted.regimes})
        self.assertTrue(hinted.exact)
        self.assertEqual(hinted.deviants, [])


class TestEvidenceCapture(unittest.TestCase):
    """§10.5 -- evidence attaches on every return path; determinism preserved."""

    def _clean_exact_tob(self):
        yms = month_range(1950, 1, 2009, 12)
        s_era = fp32.f32(-0.30)
        qcu, qcf = synth(70, yms, lambda y, m: OFF_17[m], lambda y, m: s_era)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNEV1")
        self.assertTrue(sol.exact)
        return sol

    def test_evidence_attached_on_clean_exact_tob(self):
        sol = self._clean_exact_tob()
        self.assertIsNotNone(
            sol.evidence, "clean-exact early return must carry evidence"
        )
        ev = sol.evidence
        self.assertEqual(ev["kind"], "tob")
        self.assertEqual(ev["solver_version"], rs.SOLVER_EVIDENCE_VERSION)
        self.assertEqual(len(ev["regimes"]), 1)
        r = ev["regimes"][0]
        self.assertEqual(r["code"], "17HR")
        self.assertIn("17HR", r["ambiguous_codes"])
        self.assertEqual(r["boundary"]["kind"], "record-start")
        # Full-record single regime, no holes -> one run, coverage = all months.
        self.assertEqual(len(ev["evidence_runs"]), 1)
        self.assertEqual(r["n_constrained"], 720)
        self.assertEqual(len(r["constrained_runs"]), 1)

    def test_to_dict_is_additive_only(self):
        sol = self._clean_exact_tob()
        d = sol.to_dict()
        self.assertIn("evidence", d)
        without = {k: v for k, v in d.items() if k != "evidence"}
        sol.evidence = None
        self.assertEqual(sol.to_dict(), without)

    def test_evidence_holes_visible(self):
        # Two regimes with a data hole inside the first -> constrained_runs
        # of the first regime split around the hole.
        yms = [
            ym
            for ym in month_range(1950, 1, 1979, 12)
            if not ((1960, 1) <= ym <= (1961, 12))  # 2-year hole
        ] + month_range(1980, 1, 1999, 12)
        s_era = fp32.f32(-0.30)

        def tobo_fn(y, m):
            return OFF_17[m] if (y, m) < (1980, 1) else OFF_07[m]

        qcu, qcf = synth(71, yms, tobo_fn, lambda y, m: s_era)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNEV2")
        self.assertTrue(sol.exact, msg=f"cost={sol.cost} dev={sol.deviants[:5]}")
        ev = sol.evidence
        self.assertEqual([r["code"] for r in ev["regimes"]], ["17HR", "07HR"])
        first = ev["regimes"][0]
        self.assertEqual(len(first["constrained_runs"]), 2, first["constrained_runs"])
        # The second regime begins right after the first's last constraint.
        self.assertEqual(ev["regimes"][1]["boundary"]["kind"], "constrained")

    def test_pha_only_evidence_normal(self):
        yms = month_range(1950, 1, 2009, 12)
        s_old = fp32.f32(-0.42)
        s_fn = lambda y, m: s_old if (y, m) < (1980, 1) else 0.0  # noqa: E731
        qcu, qcf = synth(72, yms, lambda y, m: 0, s_fn)
        sol = rs.solve_pha_only(qcu, qcf, sid="SYNEVP")
        self.assertIsNotNone(sol.evidence)
        ev = sol.evidence
        self.assertEqual(ev["kind"], "pha-only")
        self.assertEqual(len(ev["regimes"]), 1)
        r = ev["regimes"][0]
        self.assertEqual(r["code"], "24HR")
        self.assertEqual(r["ambiguous_codes"], ["24HR", "00HR"])
        self.assertEqual(tuple(r["begin"]), (1950, 1, 1))

    def test_pha_only_evidence_empty_and_infeasible_paths(self):
        # Empty common months -> no-common-months return still carries evidence.
        sol = rs.solve_pha_only({(1950, 1): 10}, {}, sid="SYNEVE")
        self.assertIsNotNone(sol.evidence)
        self.assertEqual(sol.evidence["kind"], "pha-only")
        self.assertEqual(sol.evidence["regimes"], [])

    def test_resolve_blend_ab_returns_identical_day(self):
        import types

        months = [rs._mi(1990, 1), rs._mi(1990, 2), rs._mi(1990, 3)]
        q1 = fp32.pha_qcf(100, 0.0)
        series = rs.Series(
            sid="B",
            months=months,
            t_raw=[100, 100, 100],
            q=[q1, q1, q1],
            qcf_only=[],
            visible=set(),
            visible_sorted=[],
        )
        seg = types.SimpleNamespace(idxs=[0, 1, 2], iv=rs.FULL)
        basis = types.SimpleNamespace(coord=(40.0, -90.0))

        def blend_fn(coord, ym, c_old, c_new):
            table = {}
            for d in range(2, 29):
                table[d] = {ym: 0} if d in (5, 9) else {}
            return table

        day_a = rs._resolve_blend(series, basis, 1, "24HR", "07HR", [seg], blend_fn)
        feas = []
        day_b = rs._resolve_blend(
            series, basis, 1, "24HR", "07HR", [seg], blend_fn, collect_days=feas
        )
        self.assertEqual(day_a, day_b)
        self.assertEqual(day_a, 5)
        self.assertEqual(feas, [5, 9])
        self.assertEqual(feas[0], day_a)


class TestPhase2VintageHints(unittest.TestCase):
    """§9 -- vintage-hint influence, laundering guard, retry."""

    def _mk_sol(self, regimes, cost, deviants=(), stats=None):
        return rs.Solution(
            "SYN",
            "tob",
            0,
            [rs.RegimeOut(b, None, c, bd) for (b, c, bd) in regimes],
            [],
            list(deviants),
            [],
            [],
            tuple(cost),
            not deviants,
            stats=dict(stats or {}),
        )

    def test_select_strict_improvement_keeps_natural(self):
        base = self._mk_sol(
            [((1950, 1, 1), "17HR", None)],
            (0, 1, 0, 1, 0, 0),
            deviants=[((1960, 6), "unexplained")],
        )
        hinted = self._mk_sol([((1950, 1, 1), "17HR", None)], (0, 0, 0, 1, 0, 0))
        out = rs._select_hint_influenced(base, hinted)
        self.assertIs(out, hinted)
        self.assertIsNone(out.hint_influenced)  # strict -> natural class
        self.assertIn("vintage-hint-strict-improvement", out.audits)

    def test_select_equal_rank_different_is_policy(self):
        base = self._mk_sol(
            [((1950, 1, 1), "17HR", None), ((1970, 1, 1), "07HR", None)],
            (0, 0, 0, 2, 0, 0),
        )
        hinted = self._mk_sol(
            [((1950, 1, 1), "17HR", None), ((1970, 1, 1), "12HR", None)],
            (0, 0, 0, 2, 0, 0),
        )
        out = rs._select_hint_influenced(base, hinted)
        self.assertIs(out, hinted)
        self.assertEqual(out.hint_influenced, {(1970, 1, 1)})  # only the differ
        self.assertIn("vintage-hint-policy-adoption", out.audits)

    def test_select_equal_rank_identical_keeps_base(self):
        regs = [((1950, 1, 1), "17HR", None)]
        base = self._mk_sol(regs, (0, 0, 0, 1, 0, 0))
        hinted = self._mk_sol(regs, (0, 0, 0, 1, 0, 0))
        out = rs._select_hint_influenced(base, hinted)
        self.assertIs(out, base)

    def test_select_never_worse(self):
        base = self._mk_sol([((1950, 1, 1), "17HR", None)], (0, 0, 0, 1, 0, 0))
        hinted = self._mk_sol(
            [((1950, 1, 1), "07HR", None)],
            (0, 2, 0, 1, 0, 0),
            deviants=[((1955, 1), "unexplained")],
        )
        out = rs._select_hint_influenced(base, hinted)
        self.assertIs(out, base)

    def test_zero_regression_vintage_hints_none(self):
        yms = month_range(1950, 1, 2009, 12)
        s_era = fp32.f32(-0.30)
        qcu, qcf = synth(80, yms, lambda y, m: OFF_17[m], lambda y, m: s_era)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        a = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNZR")
        b = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNZR", vintage_hints=[])
        da, db = a.to_dict(), b.to_dict()
        da.pop("evidence", None)
        db.pop("evidence", None)
        self.assertEqual(da, db)

    def test_vintage_hint_matching_reading_no_influence(self):
        # Vintage hint that matches the reading the base already finds must
        # not mark anything (equal rank, identical structure).
        yms = month_range(1950, 1, 2009, 12)
        s_era = fp32.f32(-0.30)

        def tobo_fn(y, m):
            return OFF_17[m] if (y, m) < (1980, 1) else OFF_07[m]

        qcu, qcf = synth(81, yms, tobo_fn, lambda y, m: s_era)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        base = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNVM")
        self.assertEqual([r.code for r in base.regimes], ["17HR", "07HR"])
        vh = [((1980, 1, 1), "07HR")]  # matches the true boundary/code
        out = rs.solve_tob_station(
            qcu, qcf, [basis], None, sid="SYNVM", vintage_hints=vh
        )
        self.assertEqual([r.code for r in out.regimes], ["17HR", "07HR"])
        self.assertIsNone(out.hint_influenced)  # no tie was tipped

    def test_repackage_incumbent_round_trip(self):
        # §9.4 reconstruction: repackaging a clean solution reproduces its
        # regimes and deviant set (the skip-guard's precondition).
        yms = month_range(1950, 1, 2009, 12)
        s_era = fp32.f32(-0.30)

        def tobo_fn(y, m):
            return OFF_17[m] if (y, m) < (1980, 1) else OFF_07[m]

        qcu, qcf = synth(82, yms, tobo_fn, lambda y, m: s_era)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        series = rs.prepare_series("SYNRP", qcu, qcf, None)
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNRP")
        self.assertTrue(sol.exact)
        repacked = rs._repackage_incumbent(series, basis, sol, [], None, "SYNRP")
        self.assertIsNotNone(repacked)
        self.assertEqual(
            [(r.begin, r.code) for r in repacked.regimes],
            [(r.begin, r.code) for r in sol.regimes],
        )
        self.assertEqual(rs._dev_month_set(repacked), rs._dev_month_set(sol))

    def test_end_to_end_policy_adoption_and_demotion(self):
        import tob_hints

        # 17HR and 18HR share a SEASONAL (non-constant) winter pattern but
        # differ in summer.  An older winter-only regime is offset-ambiguous
        # between them; a vintage 18HR hint tips the equal-cost tie.
        A = {
            1: -6,
            2: -4,
            3: -7,
            4: -6,
            5: -5,
            6: -4,
            7: -3,
            8: -3,
            9: -4,
            10: -5,
            11: -5,
            12: -8,
        }
        B = dict(A)
        B.update({6: -9, 7: -9, 8: -9})
        yms = sorted(
            set(
                [(y, m) for y in range(1950, 1970) for m in (12, 1, 2)]
                + [(y, m) for y in range(1970, 2000) for m in range(1, 13)]
            )
        )

        def tobo(y, m):
            return A[m] if y < 1970 else OFF_07[m]

        qcu, qcf = synth(90, yms, tobo, lambda y, m: fp32.f32(-0.3))
        basis = make_basis(yms, {"17HR": A, "18HR": B, "07HR": OFF_07, "24HR": OFF_24})

        base = rs.solve_tob_station(qcu, qcf, [basis], None, sid="PA")
        self.assertEqual([r.code for r in base.regimes], ["17HR", "07HR"])
        self.assertIsNone(base.hint_influenced)

        hinted = rs.solve_tob_station(
            qcu, qcf, [basis], None, sid="PA", vintage_hints=[((1950, 1, 1), "18HR")]
        )
        self.assertEqual([r.code for r in hinted.regimes], ["18HR", "07HR"])
        self.assertEqual(hinted.hint_influenced, {(1950, 1, 1)})
        self.assertIn("vintage-hint-policy-adoption", hinted.audits)
        # It is a POLICY adoption: equal rank, not strictly better.
        self.assertEqual(rs._rank_key(base), rs._rank_key(hinted))

        # Derivation demotes the tipped regime; the untouched 07HR stays natural.
        h = tob_hints.hints_from_solution(
            "PA", hinted.to_dict(), qcu, qcf, base_name="data"
        )
        by_code = {r.code: r.evidence.cls for r in h.regimes}
        self.assertEqual(by_code["18HR"], "residual-proven-hinted")
        self.assertEqual(by_code["07HR"], "residual-proven")

    def test_retry_noop_without_applicable_hint(self):
        yms = month_range(1950, 1, 2009, 12)
        s_era = fp32.f32(-0.30)
        qcu, qcf = synth(83, yms, lambda y, m: OFF_17[m], lambda y, m: s_era)
        basis = make_basis(yms, {"17HR": OFF_17, "07HR": OFF_07, "24HR": OFF_24})
        series = rs.prepare_series("SYNNO", qcu, qcf, None)
        sol = rs.solve_tob_station(qcu, qcf, [basis], None, sid="SYNNO")
        # A hint far from any boundary/deviant -> retry returns the incumbent.
        out = rs._hinted_boundary_retry(
            series, basis, sol, [((1999, 1, 1), "07HR")], None, [], "SYNNO"
        )
        self.assertEqual(
            [(r.begin, r.code) for r in out.regimes],
            [(r.begin, r.code) for r in sol.regimes],
        )


if __name__ == "__main__":
    unittest.main()
