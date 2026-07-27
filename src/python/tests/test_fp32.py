#!/usr/bin/env python3
"""Tests for fp32: float32 emulation and exact PHA interval solving."""

import math
import os
import random
import struct
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import fp32  # noqa: E402
from fp32 import (  # noqa: E402
    FLT_MAX,
    Interval,
    f32,
    f32_add,
    f32_div,
    f32_mul,
    f32_nint,
    f32_sub,
    intersect,
    nextafter32,
    pha_qcf,
    pha_sum_interval,
    solve_segment,
    tob_a_eff_interval,
    tob_adj_interval,
    tob_apply,
    tob_blend_adj,
    tob_fraction_region,
    tob_month_adj,
    tob_multi_blend_adj,
    tob_table_interval,
)


class TestF32Primitives(unittest.TestCase):
    def test_roundtrip_exact_values(self):
        for v in (0.0, -0.0, 1.0, 1.5, -2.75, 100.0, -9999.0, 0.5):
            self.assertEqual(f32(v), v)

    def test_roundtrip_inexact(self):
        self.assertNotEqual(f32(0.1), 0.1)
        self.assertEqual(f32(0.1), struct.unpack("<f", struct.pack("<f", 0.1))[0])

    def test_overflow_to_inf(self):
        self.assertTrue(math.isinf(f32(1e39)))
        self.assertTrue(math.isinf(f32(-1e39)))

    def test_div_known_value(self):
        # 1/3 correctly rounded to float32.
        self.assertEqual(f32_div(1.0, 3.0), f32(0.3333333432674408))

    def test_binary_ops_round_to_f32(self):
        a, b = f32(0.1), f32(0.2)
        self.assertEqual(f32_add(a, b), f32(a + b))
        self.assertEqual(f32_sub(a, b), f32(a - b))
        self.assertEqual(f32_mul(a, b), f32(a * b))

    def test_nextafter32(self):
        up = nextafter32(1.0, 2.0)
        self.assertEqual(up, f32(1.0000001192092896))
        self.assertEqual(nextafter32(up, 0.0), 1.0)
        self.assertEqual(nextafter32(5.0, 5.0), 5.0)
        # Across zero: dense, monotone.
        below = nextafter32(0.0, -1.0)
        self.assertLess(below, 0.0)
        self.assertEqual(nextafter32(below, 1.0), 0.0)


class TestNint(unittest.TestCase):
    def test_halves_away_from_zero(self):
        self.assertEqual(f32_nint(0.5), 1)
        self.assertEqual(f32_nint(-0.5), -1)
        self.assertEqual(f32_nint(2.5), 3)
        self.assertEqual(f32_nint(-2.5), -3)
        self.assertEqual(f32_nint(36.5), 37)
        self.assertEqual(f32_nint(-36.5), -37)

    def test_nearest(self):
        self.assertEqual(f32_nint(f32(0.49999997)), 0)
        self.assertEqual(f32_nint(f32(-0.49999997)), 0)
        self.assertEqual(f32_nint(f32(36.50161)), 37)
        self.assertEqual(f32_nint(f32(36.499)), 36)
        self.assertEqual(f32_nint(0.0), 0)
        self.assertEqual(f32_nint(-0.0), 0)

    def test_large_magnitude_integers(self):
        x = f32(2.0**25 + 4)
        self.assertEqual(f32_nint(x), int(x))


class TestPhaQcf(unittest.TestCase):
    def test_zero_s_is_identity(self):
        for t in range(-5000, 5001, 7):
            self.assertEqual(pha_qcf(t, 0.0), t, msg=f"t={t}")

    def test_simple_shift(self):
        # S = 1.0 degC shifts by exactly -100 cents for representable cases.
        self.assertEqual(pha_qcf(0, 1.0), -100)
        self.assertEqual(pha_qcf(250, 1.0), 150)


def _scan_run_check(test, iv, predicate):
    """Endpoints must satisfy predicate; their outside neighbours must not."""
    test.assertTrue(predicate(iv.lo))
    test.assertTrue(predicate(iv.hi))
    below = nextafter32(iv.lo, -math.inf)
    above = nextafter32(iv.hi, math.inf)
    if not math.isinf(below):
        test.assertFalse(predicate(below))
    if not math.isinf(above):
        test.assertFalse(predicate(above))


class TestPhaSumInterval(unittest.TestCase):
    def test_bruteforce_random(self):
        rng = random.Random(42)
        for _ in range(300):
            t = rng.randint(-4500, 4500)
            s_true = f32(rng.uniform(-5.0, 5.0))
            q = pha_qcf(t, s_true)
            iv = pha_sum_interval(t, q)
            self.assertIsNotNone(iv, msg=f"t={t} s={s_true!r}")
            self.assertTrue(iv.contains(s_true), msg=f"t={t} s={s_true!r}")
            _scan_run_check(self, iv, lambda s, t=t, q=q: pha_qcf(t, s) == q)

    def test_knife_edge_neighbourhood(self):
        # Around S values that put (t/100f - S)*100f exactly near x.5.
        rng = random.Random(7)
        for _ in range(50):
            t = rng.randint(-4500, 4500)
            base = f32(rng.uniform(-3.0, 3.0))
            # Walk a small neighbourhood in ulps and confirm the interval
            # machinery agrees with direct evaluation everywhere.
            s = base
            for _ in range(20):
                s = nextafter32(s, math.inf)
            s = base
            for _ in range(40):
                q = pha_qcf(t, s)
                iv = pha_sum_interval(t, q)
                self.assertIsNotNone(iv)
                self.assertTrue(iv.contains(s))
                s = nextafter32(s, math.inf)

    def test_unreachable_q_is_none(self):
        # 2**25 - 1 is not attainable: float32 spacing there is 4, and nint
        # of those values yields the (even) representable integers only.
        self.assertIsNone(pha_sum_interval(0, 2**25 - 1))

    def test_t_zero(self):
        iv = pha_sum_interval(0, 0)
        self.assertIsNotNone(iv)
        self.assertTrue(iv.contains(0.0))
        _scan_run_check(self, iv, lambda s: pha_qcf(0, s) == 0)

    def test_extreme_s_no_crash(self):
        # Ensure the search handles the overflow sentinels at the domain ends.
        iv = pha_sum_interval(4500, -3000)
        self.assertIsNotNone(iv)
        _scan_run_check(self, iv, lambda s: pha_qcf(4500, s) == -3000)

    def test_monotone_contiguous_random_windows(self):
        rng = random.Random(1234)
        for _ in range(30):
            t = rng.randint(-4500, 4500)
            s = f32(rng.uniform(-6.0, 6.0))
            prev = None
            for _ in range(400):
                cur = pha_qcf(t, s)
                if prev is not None:
                    self.assertGreaterEqual(prev, cur, msg=f"t={t} s={s!r}")
                prev = cur
                s = nextafter32(s, math.inf)


class TestSolveSegment(unittest.TestCase):
    def test_known_s_recovered(self):
        rng = random.Random(99)
        for _ in range(100):
            s_true = f32(rng.uniform(-4.0, 4.0))
            ts = [rng.randint(-4000, 4000) for _ in range(rng.randint(3, 40))]
            pairs = [(t, pha_qcf(t, s_true)) for t in ts]
            iv = solve_segment(pairs)
            self.assertIsNotNone(iv, msg=f"s={s_true!r}")
            self.assertTrue(iv.contains(s_true))
            # Every endpoint of the solved interval reproduces every pair.
            for t, q in pairs:
                self.assertEqual(pha_qcf(t, iv.lo), q)
                self.assertEqual(pha_qcf(t, iv.hi), q)

    def test_contradiction_is_none(self):
        self.assertIsNone(solve_segment([(1000, 900), (1000, 1100)]))

    def test_changepoint_detected(self):
        # First half S=0.5, second half S=1.5: no single S fits.
        s_a, s_b = f32(0.5), f32(1.5)
        pairs = [(t, pha_qcf(t, s_a)) for t in (100, 200, 300)]
        pairs += [(t, pha_qcf(t, s_b)) for t in (400, 500, 600)]
        self.assertIsNone(solve_segment(pairs))

    def test_empty_input_unconstrained(self):
        iv = solve_segment([])
        self.assertEqual((iv.lo, iv.hi), (-FLT_MAX, FLT_MAX))

    def test_narrowing(self):
        # More pairs must never widen the interval.
        s_true = f32(1.2345)
        ts = list(range(-2000, 2001, 250))
        pairs = [(t, pha_qcf(t, s_true)) for t in ts]
        wide = solve_segment(pairs[:2])
        narrow = solve_segment(pairs)
        self.assertLessEqual(wide.lo, narrow.lo)
        self.assertGreaterEqual(wide.hi, narrow.hi)


class TestIntersect(unittest.TestCase):
    def test_overlap(self):
        iv = intersect([Interval(0.0, 2.0), Interval(1.0, 3.0)])
        self.assertEqual((iv.lo, iv.hi), (1.0, 2.0))

    def test_disjoint(self):
        self.assertIsNone(intersect([Interval(0.0, 1.0), Interval(2.0, 3.0)]))

    def test_empty_iterable(self):
        self.assertIsNone(intersect([]))

    def test_touching_endpoints(self):
        iv = intersect([Interval(0.0, 1.0), Interval(1.0, 2.0)])
        self.assertEqual((iv.lo, iv.hi), (1.0, 1.0))


class TestTobAdjInterval(unittest.TestCase):
    def test_positive_offset(self):
        iv = tob_adj_interval(3)
        self.assertIsNotNone(iv)
        self.assertTrue(iv.contains(3.0))
        # 2.5 rounds away from zero to 3, so it is inside; 3.5 rounds to 4.
        self.assertEqual(iv.lo, 2.5)
        self.assertLess(iv.hi, 3.5)
        _scan_run_check(self, iv, lambda a: f32_nint(a) == 3)

    def test_negative_offset(self):
        iv = tob_adj_interval(-3)
        self.assertTrue(iv.contains(-3.0))
        self.assertEqual(iv.hi, -2.5)
        _scan_run_check(self, iv, lambda a: f32_nint(a) == -3)

    def test_zero_offset(self):
        iv = tob_adj_interval(0)
        self.assertTrue(iv.contains(0.0))
        self.assertTrue(iv.contains(-0.0))
        self.assertLess(iv.lo, 0.0)
        self.assertGreater(iv.hi, 0.0)
        _scan_run_check(self, iv, lambda a: f32_nint(a) == 0)

    def test_random_offsets_bruteforce(self):
        rng = random.Random(5)
        for _ in range(50):
            k = rng.randint(-2000, 2000)
            iv = tob_adj_interval(k)
            self.assertIsNotNone(iv)
            self.assertTrue(iv.contains(float(k)))
            _scan_run_check(self, iv, lambda a, k=k: f32_nint(a) == k)


def _mirror_adj(segments, days_tot):
    """Independent inline mirror of the CURRENT get_monthly_adj:
    per-term division (ws += f32(f32(f32(n)*a)/N)), no final divide."""
    ws = f32(0.0)
    for seg_days, a in segments:
        if a is None:
            continue
        term = f32(f32(f32(float(seg_days)) * f32(a)) / f32(float(days_tot)))
        ws = f32(ws + term)
    return ws


def _mirror_apply(v_cents, adj):
    """Independent inline mirror of the CURRENT apply_adjustments chain:
    rval = real(v)/100.; rval = rval - adj; nint(rval*100.)."""
    rval = f32(f32(float(v_cents)) / f32(100.0))
    rval = f32(rval - f32(adj))
    return f32_nint(f32(rval * f32(100.0)))


class TestTobForwardModel(unittest.TestCase):
    def test_apply_is_pha_chain(self):
        rng = random.Random(11)
        for _ in range(500):
            v = rng.randint(-4500, 4500)
            a = f32(rng.uniform(-1.5, 1.5))
            self.assertEqual(tob_apply(v, a), pha_qcf(v, a))
            self.assertEqual(tob_apply(v, a), _mirror_apply(v, a))

    def test_month_adj_cross_check(self):
        rng = random.Random(12)
        for _ in range(500):
            a = f32(rng.uniform(-1.5, 1.5))
            for days in (28, 29, 30, 31):
                adj = tob_month_adj(a, days)
                self.assertEqual(adj, _mirror_adj([(days, a)], days))
                self.assertEqual(
                    adj,
                    f32(f32(f32(float(days)) * a) / f32(float(days))),
                )

    def test_blend_adj_cross_check(self):
        rng = random.Random(13)
        for _ in range(500):
            al = f32(rng.uniform(-1.5, 1.5))
            ar = f32(rng.uniform(-1.5, 1.5))
            dt = rng.choice((28, 29, 30, 31))
            d = rng.randint(2, dt)
            dl, dr = d - 1, dt - (d - 1)
            self.assertEqual(
                tob_blend_adj(al, ar, dl, dr, dt),
                _mirror_adj([(dl, al), (dr, ar)], dt),
            )

    def test_multi_segment_and_none(self):
        rng = random.Random(14)
        for _ in range(200):
            dt = 31
            segs = []
            left = dt
            while left > 0:
                nd = rng.randint(1, left)
                a = None if rng.random() < 0.3 else f32(rng.uniform(-1.0, 1.0))
                segs.append((nd, a))
                left -= nd
            self.assertEqual(tob_multi_blend_adj(segs, dt), _mirror_adj(segs, dt))
        # All-None (whole month of 24HR): zero adjustment, output == input.
        self.assertEqual(tob_multi_blend_adj([(31, None)], 31), 0.0)
        for v in (-4321, 0, 987):
            self.assertEqual(tob_apply(v, 0.0), v)

    def test_monotone_output_in_each_argument(self):
        rng = random.Random(15)
        for _ in range(20):
            v = rng.randint(-4000, 4000)
            ar = f32(rng.uniform(-1.0, 1.0))
            al = f32(rng.uniform(-1.0, 1.0))
            dt = 31
            dl = rng.randint(1, 30)
            dr = dt - dl
            # Walk a_left upward by ulps: output t must be non-increasing.
            a = al
            prev = None
            for _ in range(300):
                cur = tob_apply(v, tob_blend_adj(a, ar, dl, dr, dt))
                if prev is not None:
                    self.assertGreaterEqual(prev, cur)
                prev = cur
                a = nextafter32(a, math.inf)
            # Same along a_right.
            a = ar
            prev = None
            for _ in range(300):
                cur = tob_apply(v, tob_blend_adj(al, a, dl, dr, dt))
                if prev is not None:
                    self.assertGreaterEqual(prev, cur)
                prev = cur
                a = nextafter32(a, math.inf)

    def test_value_dependence(self):
        # The corrected model has NO data-independent offset: the same adj
        # shifts different values by different whole cents at half-cent
        # knife edges.
        adj = f32(0.005)  # exactly half a cent
        offsets = {tob_apply(v, adj) - v for v in range(-2000, 2001, 7)}
        self.assertGreater(len(offsets), 1)


class TestTobFractionRecovery(unittest.TestCase):
    def test_a_eff_recovery_70_pairs_generic(self):
        # ~70 (v, t) pairs of one calendar month, solved by the shared PHA
        # interval machinery.  In exact arithmetic (v/100 - a)*100 = v - 100a,
        # so every pair's knife edge sits at the SAME fractional position:
        # generically the recovery brackets a_eff to one cent minus the
        # float32 wobble spread (~ppm), no better.
        rng = random.Random(21)
        widths = []
        for _ in range(20):
            a = f32(rng.uniform(-1.5, 1.5))
            a_eff = tob_month_adj(a, 31)
            vs = [rng.randint(-4500, 4500) for _ in range(70)]
            pairs = [(v, tob_apply(v, a_eff)) for v in vs]
            iv = tob_a_eff_interval(pairs)
            self.assertIsNotNone(iv)
            self.assertTrue(iv.contains(a_eff))
            for v, t in pairs[:5]:
                self.assertEqual(tob_apply(v, iv.lo), t)
                self.assertEqual(tob_apply(v, iv.hi), t)
            widths.append(iv.hi - iv.lo)
        self.assertLessEqual(max(widths), 0.01)
        self.assertLess(max(widths), 0.01)  # strictly narrowed by wobble

    def test_a_eff_recovery_knife_edge(self):
        # When 100*a_eff sits within the float32 wobble (~6e-4 cents) of a
        # half-cent boundary, different v round in different directions
        # (value-dependence) and the intersection collapses to near a single
        # float32 value -- THE fraction-recovery payoff, and the explanation
        # of the observed rare per-(station, month) knife-edge jitter.
        rng = random.Random(25)
        for base in (0.125001, 0.1250005, 0.0850002, 0.33500015):
            a_eff = f32(base)
            vs = [rng.randint(-4500, 4500) for _ in range(70)]
            pairs = [(v, tob_apply(v, a_eff)) for v in vs]
            offsets = {t - v for v, t in pairs}
            self.assertGreater(len(offsets), 1, msg=f"a_eff={a_eff!r}")
            iv = tob_a_eff_interval(pairs)
            self.assertIsNotNone(iv)
            self.assertTrue(iv.contains(a_eff))
            self.assertLess(iv.hi - iv.lo, 1e-5, msg=f"a_eff={a_eff!r}")

    def test_a_eff_mixed_codes_rejected(self):
        # Pairs generated under two different a_eff values must contradict.
        a1 = f32(0.31)
        a2 = f32(0.34)
        pairs = [(v, tob_apply(v, a1)) for v in range(100, 1500, 100)]
        pairs += [(v, tob_apply(v, a2)) for v in range(1600, 3000, 100)]
        self.assertIsNone(tob_a_eff_interval(pairs))

    def test_table_interval_exact_single_n(self):
        rng = random.Random(22)
        for _ in range(100):
            a = f32(rng.uniform(-1.5, 1.5))
            n = rng.choice((28, 29, 30, 31))
            eff = tob_month_adj(a, n)
            iv = tob_table_interval({n: Interval(eff, eff)})
            self.assertIsNotNone(iv)
            self.assertTrue(iv.contains(a))
            # Endpoints are extreme: one ulp outside no longer maps to eff.
            _scan_run_check(
                self,
                iv,
                lambda x, n=n, e=eff: tob_month_adj(x, n) == e,
            )

    def test_leap_feb_intersection(self):
        rng = random.Random(23)
        for _ in range(50):
            a = f32(rng.uniform(-1.5, 1.5))
            eff28 = tob_month_adj(a, 28)
            eff29 = tob_month_adj(a, 29)
            only28 = tob_table_interval({28: Interval(eff28, eff28)})
            both = tob_table_interval(
                {28: Interval(eff28, eff28), 29: Interval(eff29, eff29)}
            )
            self.assertIsNotNone(both)
            self.assertTrue(both.contains(a))
            self.assertLessEqual(only28.lo, both.lo)
            self.assertGreaterEqual(only28.hi, both.hi)

    def test_full_chain_feb_recovery(self):
        # (v, t) pairs from leap and non-leap Februaries -> a_eff intervals
        # per month length -> joint inversion contains the hidden a_table
        # value.
        rng = random.Random(24)
        a = f32(rng.uniform(-1.0, 1.0))
        by_n = {}
        for n in (28, 29):
            a_eff = tob_month_adj(a, n)
            vs = [rng.randint(-4000, 4000) for _ in range(35)]
            iv = tob_a_eff_interval([(v, tob_apply(v, a_eff)) for v in vs])
            self.assertIsNotNone(iv)
            by_n[n] = iv
        joint = tob_table_interval(by_n)
        self.assertIsNotNone(joint)
        self.assertTrue(joint.contains(a))

    def test_table_interval_inconsistent(self):
        self.assertIsNone(
            tob_table_interval(
                {
                    28: Interval(f32(0.1), f32(0.1)),
                    29: Interval(f32(-0.1), f32(-0.1)),
                }
            )
        )


class TestTobFractionRegion(unittest.TestCase):
    def _recover(self, al, ar, dt=31):
        rng = random.Random(4242)
        # Whole-month boxes via the primary 1D mechanism for each code.
        boxes = []
        for a in (al, ar):
            a_eff = tob_month_adj(a, dt)
            vs = [rng.randint(-4000, 4000) for _ in range(30)]
            eff_iv = tob_a_eff_interval([(v, tob_apply(v, a_eff)) for v in vs])
            self.assertIsNotNone(eff_iv)
            box = tob_table_interval({dt: eff_iv})
            self.assertIsNotNone(box)
            self.assertTrue(box.contains(a))
            boxes.append(box)
        cons = []
        for d in range(2, dt + 1):
            dl, dr = d - 1, dt - (d - 1)
            v = rng.randint(-4000, 4000)
            t = tob_apply(v, tob_blend_adj(al, ar, dl, dr, dt))
            cons.append((dl, dr, dt, v, t))
        return tob_fraction_region(cons, boxes[0], boxes[1]), cons

    def test_synthetic_recovery(self):
        rng = random.Random(31)
        for _ in range(5):
            al = f32(rng.uniform(-1.0, 1.0))
            ar = f32(rng.uniform(-1.0, 1.0))
            region, _ = self._recover(al, ar)
            self.assertTrue(region.feasible)
            self.assertTrue(region.left.contains(al))
            self.assertTrue(region.right.contains(ar))

    def test_infeasible_contradiction(self):
        box = Interval(f32(-0.01), f32(0.01))
        cons = [
            (10, 21, 31, 0, 500),  # output far outside the box's reach
        ]
        region = tob_fraction_region(cons, box, box)
        self.assertFalse(region.feasible)
        self.assertIsNone(region.left)
        self.assertIsNone(region.best_fraction())

    def test_no_constraints_returns_box(self):
        box_l = Interval(f32(0.1), f32(0.2))
        box_r = Interval(f32(-0.2), f32(-0.1))
        region = tob_fraction_region([], box_l, box_r)
        self.assertTrue(region.feasible)
        self.assertEqual((region.left.lo, region.left.hi), (box_l.lo, box_l.hi))
        self.assertEqual((region.right.lo, region.right.hi), (box_r.lo, box_r.hi))

    def test_witness_reproduces_observations(self):
        al = f32(0.7654)
        ar = f32(-0.2345)
        region, cons = self._recover(al, ar)
        self.assertIsNotNone(region.witness)
        bl, br = region.best_fraction()
        for dl, dr, dt, v, t in cons:
            self.assertEqual(tob_apply(v, tob_blend_adj(bl, br, dl, dr, dt)), t)


if __name__ == "__main__":
    unittest.main()
