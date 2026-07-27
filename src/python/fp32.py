#!/usr/bin/env python3
"""IEEE-754 float32 emulation and exact interval solving (pure stdlib).

Shared exact-arithmetic kernel for BOTH sides of the reconstruction:

PHA side -- reproduces, bit-for-bit, the float32 chain PHAMain uses to apply
a piecewise-constant adjustment sum S to a TOB value:

    qcf_cents = NINT( (t_cents/100.0f - S) * 100.0f )      [all float32]

and inverts it: given observed (t_cents, qcf_cents) pairs, recover the exact
closed interval of float32 S values consistent with ALL of them.  An empty
intersection is a proof that no single S explains the segment (changepoint or
wrong TOB hypothesis) -- there are no tolerances anywhere.

TOB side -- reproduces the float32 chain TOBMain uses to turn a_table biases
into the integer offset subtracted from a month (apply_adjustments +
get_monthly_adj, src/f95/TOBMain.f95:713-804):

    ws  = 0.0f;  ws += real(seg_days) * a_table(m, code)   [per segment]
    adj = ws / real(days_tot)
    offset = tob - raw = -NINT( adj * 100.0f )             [all float32]

and inverts it: a whole-month observation only brackets a to a 1-cent
interval, but blend observations (a mid-month switch at day d weights the two
codes' floats differently) constrain the same pair (a_left, a_right) with
many different weightings, so intersecting the feasible sets recovers the
fractional cents of both a_table values -- the TOB fraction-recovery
mechanism used with the fake-inventory basis/blend runs.

float32 emulation notes:
  * f32(x) rounds a Python float (binary64) to binary32 via struct, which
    uses the platform's correctly-rounded double->float conversion
    (round-to-nearest-even).
  * Binary ops are computed in binary64 and then rounded to binary32.  For
    +, -, * the binary64 result of binary32 operands is exact; for / the
    double rounding is innocuous because binary64's 53 bits >= 2*24+2
    (Figueroa's theorem), so all four match a native float32 operation.
  * f32_nint matches Fortran NINT on a float32 argument: round to nearest,
    halves away from zero.  The float32 argument converts to binary64
    exactly, and x +/- 0.5 is exact in binary64 for |x| < 2**52, so the
    Python computation is the mathematically exact rounding of the float32
    value.  For |x| >= 2**24 every float32 is an integer and int(x) is used.
"""

import math
import struct
from dataclasses import dataclass
from typing import Iterable, List, Optional, Sequence, Tuple

_F = struct.Struct("<f")
_I = struct.Struct("<I")

# Largest finite float32.
FLT_MAX = _F.unpack(_I.pack(0x7F7FFFFF))[0]

# Sentinel returned by the extended qcf evaluator when the float32 chain
# overflows to +/-inf; large enough to compare correctly against any cents.
_HUGE = 1 << 62


# ---------------------------------------------------------------------------
# float32 primitives
# ---------------------------------------------------------------------------


def f32(x: float) -> float:
    """Round a Python float to the nearest binary32 value (as a float)."""
    try:
        return _F.unpack(_F.pack(x))[0]
    except OverflowError:
        return math.copysign(math.inf, x)


def f32_add(a: float, b: float) -> float:
    return f32(f32(a) + f32(b))


def f32_sub(a: float, b: float) -> float:
    return f32(f32(a) - f32(b))


def f32_mul(a: float, b: float) -> float:
    return f32(f32(a) * f32(b))


def f32_div(a: float, b: float) -> float:
    return f32(f32(a) / f32(b))


def f32_nint(x: float) -> int:
    """Fortran NINT for a float32 value: nearest int, halves away from zero."""
    if math.isnan(x):
        raise ValueError("NINT(NaN)")
    if math.isinf(x):
        raise OverflowError("NINT(Inf)")
    if abs(x) >= 2.0**24:
        # Every float32 of this magnitude is an exact integer.
        return int(x)
    if x >= 0.0:
        return int(math.floor(x + 0.5))
    return int(math.ceil(x - 0.5))


# ---------------------------------------------------------------------------
# Ordered-key mapping over the finite float32 line (monotone, dense)
# ---------------------------------------------------------------------------


def _bits(x: float) -> int:
    return _I.unpack(_F.pack(x))[0]


def _from_bits(b: int) -> float:
    return _F.unpack(_I.pack(b))[0]


def _key(x: float) -> int:
    """Map float32 -> int such that x < y  <=>  _key(x) < _key(y).

    (-0.0 and +0.0 get adjacent keys; both map back to a zero.)
    """
    b = _bits(x)
    if b < 0x80000000:
        return b ^ 0x80000000
    return 0xFFFFFFFF - b


def _from_key(k: int) -> float:
    if k >= 0x80000000:
        return _from_bits(k ^ 0x80000000)
    return _from_bits(0xFFFFFFFF - k)


_KEY_MIN = _key(-FLT_MAX)
_KEY_MAX = _key(FLT_MAX)


def nextafter32(x: float, toward: float) -> float:
    """The adjacent float32 value from x in the direction of toward."""
    xf = f32(x)
    tf = f32(toward)
    if xf == tf:
        return tf
    step = 1 if tf > xf else -1
    k = _key(xf) + step
    if _KEY_MIN <= k <= _KEY_MAX and _from_key(k) == xf:
        # -0.0 and +0.0 occupy adjacent keys but compare equal; skip over.
        k += step
    if k < _KEY_MIN or k > _KEY_MAX:
        return math.copysign(math.inf, tf - xf)
    return _from_key(k)


# ---------------------------------------------------------------------------
# The PHA adjustment model
# ---------------------------------------------------------------------------


def pha_qcf(t_cents: int, s: float) -> int:
    """qcf cents for TOB cents t under segment sum S: nint((t/100f - S)*100f)."""
    v = f32_div(float(t_cents), 100.0)
    d = f32_sub(v, f32(s))
    m = f32_mul(d, 100.0)
    return f32_nint(m)


def _qcf_ext(t_cents: int, s: float) -> int:
    """pha_qcf with overflow clamped to comparable sentinels."""
    v = f32_div(float(t_cents), 100.0)
    d = f32_sub(v, s)
    m = f32_mul(d, 100.0)
    if math.isinf(m):
        return -_HUGE if m < 0 else _HUGE
    return f32_nint(m)


@dataclass(frozen=True)
class Interval:
    """Closed interval [lo, hi]; endpoints are exact float32 values."""

    lo: float
    hi: float

    def contains(self, x: float) -> bool:
        return self.lo <= x <= self.hi


def _run_of_nonincreasing(g, target: int) -> Optional[Interval]:
    """Exact key-run where a non-increasing integer function equals target.

    g maps a float32 (given as a key via _from_key) to an int; g must be
    non-increasing along increasing keys.  Returns the closed interval of
    float32 values where g == target, or None.
    """
    # First key with g(key) <= target  (predicate monotone non-decreasing).
    lo_k, hi_k = _KEY_MIN, _KEY_MAX
    if g(_from_key(hi_k)) > target:
        return None
    while lo_k < hi_k:
        mid = (lo_k + hi_k) // 2
        if g(_from_key(mid)) <= target:
            hi_k = mid
        else:
            lo_k = mid + 1
    first = lo_k
    if g(_from_key(first)) != target:
        return None
    # First key with g(key) < target; the run ends one before it.
    lo_k, hi_k = first, _KEY_MAX
    if g(_from_key(hi_k)) >= target:
        last = _KEY_MAX
    else:
        while lo_k < hi_k:
            mid = (lo_k + hi_k) // 2
            if g(_from_key(mid)) < target:
                hi_k = mid
            else:
                lo_k = mid + 1
        last = lo_k - 1
    return Interval(_from_key(first), _from_key(last))


def pha_sum_interval(t_cents: int, q_cents: int) -> Optional[Interval]:
    """All float32 S with pha_qcf(t_cents, S) == q_cents, as [lo, hi]."""
    return _run_of_nonincreasing(lambda s: _qcf_ext(t_cents, s), q_cents)


def tob_adj_interval(offset: int) -> Optional[Interval]:
    """All float32 a (adjustment*100, CENTS space) with f32_nint(a) == offset.

    Diagnostic-only inversion of a bare NINT: it does NOT model TOBMain's
    days*a/days + *100 chain.  For real TOB work use tob_whole_month_interval
    (degrees) / tob_partial_month_interval / tob_fraction_region instead.
    """

    # f32_nint is non-decreasing; negate to reuse the non-increasing finder.
    def g(a: float) -> int:
        if math.isinf(a):
            return _HUGE if a < 0 else -_HUGE
        return -f32_nint(a)

    return _run_of_nonincreasing(g, -offset)


def intersect(intervals: Iterable[Interval]) -> Optional[Interval]:
    """Intersection of closed intervals; None if empty (or no intervals)."""
    lo: Optional[float] = None
    hi: Optional[float] = None
    for iv in intervals:
        if lo is None or iv.lo > lo:
            lo = iv.lo
        if hi is None or iv.hi < hi:
            hi = iv.hi
    if lo is None or hi is None or lo > hi:
        return None
    return Interval(lo, hi)


def solve_segment(pairs: Sequence[Tuple[int, int]]) -> Optional[Interval]:
    """Interval of float32 S consistent with every (t_cents, qcf_cents) pair.

    Empty input returns the unconstrained full finite range.  None means the
    pairs are jointly inconsistent with any single S (changepoint inside the
    segment, or a wrong TOB hypothesis).
    """
    result = Interval(-FLT_MAX, FLT_MAX)
    for t_cents, q_cents in pairs:
        iv = pha_sum_interval(t_cents, q_cents)
        if iv is None:
            return None
        lo = result.lo if result.lo >= iv.lo else iv.lo
        hi = result.hi if result.hi <= iv.hi else iv.hi
        if lo > hi:
            return None
        result = Interval(lo, hi)
    return result


# ---------------------------------------------------------------------------
# The TOB adjustment model (TOBMain apply_adjustments + get_monthly_adj,
# matching the genuine v3 arithmetic -- ushcn_tobs.v4a.f lines 1856,
# 1076/1201/1356, 1415, ~1174)
# ---------------------------------------------------------------------------
#
# Fortran operation order (src/f95/TOBMain.f95 apply_adjustments +
# get_monthly_adj), mirrored exactly:
#
#   weighted_sum = 0.
#   per segment (chronological):
#       weighted_sum = weighted_sum + real(seg_days)*a_table(m,code)/real(N)
#     (PER-TERM division: f32 multiply, f32 divide by the leap-aware month
#      length N, then f32 add; segments whose code is 24 or not an hourly
#      code contribute NOTHING -- the accumulate is skipped)
#   adj = weighted_sum                            (no final divide)
#
#   application is VALUE-DEPENDENT (same functional form as PHA):
#   rval = real(v_cents)/100.;  rval = rval - adj;  v' = nint(rval*100.)
#
# There is NO data-independent integer offset: the same adj can shift
# different v_cents by different whole cents at half-cent knife edges.  The
# offset convention (t - v) survives only as an observation, not a model
# primitive.  Whole-month single code: a_eff = f32(f32(f32(n)*a)/n), which is
# NOT an identity round-trip in float32 and differs between leap/non-leap
# February for the same a.


def tob_apply(v_cents: int, adj: float) -> int:
    """Output integer cents for input v_cents under monthly adjustment adj.

    nint(f32(f32(f32(float(v))/100 - adj)*100)) -- IDENTICAL chain to the PHA
    application (pha_qcf) with adj in place of the segment sum S; implemented
    as a direct alias so both sides share one arithmetic kernel.
    """
    return pha_qcf(v_cents, adj)


def tob_multi_blend_adj(
    segments: Sequence[Tuple[int, Optional[float]]], days_tot: int
) -> float:
    """The float32 monthly adjustment for a month split into (seg_days, a)
    segments -- get_monthly_adj's per-term-division accumulation.

    a is the a_table value in degrees for the segment's code, or None for a
    zero-bias segment (code 24 / non-hourly): the Fortran skips those
    entirely, which is what None reproduces (0.0 is equivalent except for
    the sign of a -0.0 accumulator).  days_tot is the (leap-aware) length of
    the month; segment days should sum to it, but this is not enforced --
    the Fortran does no such check either.
    """
    ws = 0.0
    for seg_days, a in segments:
        if a is None:
            continue
        term = f32_div(f32_mul(float(seg_days), f32(a)), float(days_tot))
        ws = f32_add(ws, term)
    return ws


def tob_blend_adj(
    a_left: Optional[float],
    a_right: Optional[float],
    days_left: int,
    days_right: int,
    days_tot: int,
) -> float:
    """Adjustment for a month with one switch: days_left of a_left then
    days_right of a_right (switch at day days_left+1)."""
    return tob_multi_blend_adj([(days_left, a_left), (days_right, a_right)], days_tot)


def tob_month_adj(a: float, days_tot: int) -> float:
    """a_eff for a whole month under a single code: f32(f32(f32(n)*a)/n).

    NOT always equal to a (the multiply/divide round-trip can move the
    value), and leap February (n=29) can give a different a_eff than
    non-leap (n=28) for the same a_table value.
    """
    return tob_multi_blend_adj([(days_tot, a)], days_tot)


def tob_a_eff_interval(pairs: Sequence[Tuple[int, int]]) -> Optional[Interval]:
    """All float32 a_eff consistent with observed (v_cents, t_cents) pairs.

    The TOB application chain is identical to PHA's, so this is exactly
    solve_segment with a_eff in place of S: each pair brackets a_eff, and
    the intersection across ~70 years of one calendar month pins it far
    below a cent (value-dependent rounding is what makes different v values
    informative).  None = no single a_eff explains the pairs (mixed codes,
    a mid-period history change, or a wrong hypothesis).
    """
    return solve_segment(pairs)


def tob_table_interval(
    by_n: "dict[int, Interval]",
) -> Optional[Interval]:
    """Invert a_eff = f32(f32(f32(n)*a)/n) back to the a_table value a.

    by_n maps month length n (28, 29, 30, 31) to a required a_eff interval
    (e.g. from tob_a_eff_interval of that month-length's pairs).  For each n
    the map a -> a_eff is monotone non-decreasing in float32, so the exact
    preimage of [lo, hi] is found by binary search over the float32 key
    space; the results are intersected across n (leap and non-leap February
    constrain the same a_table value).  Returns None when the preimages are
    inconsistent (or any single preimage is empty).
    """

    def preimage(n: int, target: Interval) -> Optional[Interval]:
        nf = float(n)

        def eff(a: float) -> float:
            return f32(f32(f32(nf) * a) / nf)

        # First float32 a with eff(a) >= target.lo.
        lo_k, hi_k = _KEY_MIN, _KEY_MAX
        if eff(_from_key(hi_k)) < target.lo:
            return None
        while lo_k < hi_k:
            mid = (lo_k + hi_k) // 2
            if eff(_from_key(mid)) >= target.lo:
                hi_k = mid
            else:
                lo_k = mid + 1
        first = lo_k
        # Last float32 a with eff(a) <= target.hi.
        lo_k, hi_k = _KEY_MIN, _KEY_MAX
        if eff(_from_key(lo_k)) > target.hi:
            return None
        while lo_k < hi_k:
            mid = (lo_k + hi_k + 1) // 2
            if eff(_from_key(mid)) <= target.hi:
                lo_k = mid
            else:
                hi_k = mid - 1
        last = lo_k
        if first > last:
            return None
        return Interval(_from_key(first), _from_key(last))

    result: Optional[Interval] = None
    for n, target in by_n.items():
        iv = preimage(n, target)
        if iv is None:
            return None
        result = iv if result is None else intersect([result, iv])
        if result is None:
            return None
    return result


def _blend_out_ext(
    v_cents: int,
    a_left: float,
    a_right: float,
    days_left: int,
    days_right: int,
    days_tot: int,
) -> int:
    """tob_apply(v, blend adj) with overflow clamped to comparable sentinels."""
    adj = tob_multi_blend_adj([(days_left, a_left), (days_right, a_right)], days_tot)
    if math.isinf(adj):
        return -_HUGE if adj > 0 else _HUGE
    return _qcf_ext(v_cents, adj)


@dataclass(frozen=True)
class FractionRegion:
    """Feasible region for a code pair's a_table values (degrees).

    left/right are the bounding box of all surviving cells (None when
    infeasible); cells is the number of surviving cells at the final
    refinement level; witness is a point verified to reproduce every
    constraint exactly (None if none of the sampled cell points did --
    the box is then still a valid outer bound).

    Role note: under the value-dependent model the PRIMARY
    fraction-recovery mechanism is 1D -- tob_a_eff_interval over the many
    (v, t) pairs of a calendar month, then tob_table_interval.  This 2D
    region is for blend-boundary work (months containing a mid-month code
    switch), where the two codes' a_table values enter jointly; its
    precision depends on how many blend observations exist and where their
    knife edges fall.  Exact outputs for specific candidate blends should
    come from direct basis runs; this region prunes and cross-checks.
    """

    feasible: bool
    left: Optional[Interval]
    right: Optional[Interval]
    cells: int
    witness: Optional[Tuple[float, float]] = None

    def best_fraction(self) -> Optional[Tuple[float, float]]:
        """A representative (a_left, a_right): the exact witness when one was
        found, else the box midpoint (approximate -- may not reproduce every
        constraint, since the region can be a diagonal band)."""
        if not self.feasible:
            return None
        if self.witness is not None:
            return self.witness
        return (
            f32((self.left.lo + self.left.hi) / 2.0),
            f32((self.right.lo + self.right.hi) / 2.0),
        )


def tob_fraction_region(
    constraints: Sequence[Tuple[int, int, int, int, int]],
    left_box: Interval,
    right_box: Interval,
    resolution: float = 1e-8,
    max_cells: int = 4096,
) -> FractionRegion:
    """Feasible (a_left, a_right) region under blend-month observations.

    constraints: (days_left, days_right, days_tot, v_cents, t_cents) tuples,
    each asserting
        tob_apply(v_cents, tob_blend_adj(a_left, a_right, dl, dr, dt))
            == t_cents
    (the value-dependent chain).  left_box/right_box: starting
    bounds in degrees, typically from tob_table_interval of each code's
    recovered whole-month a_eff interval.

    Method: adaptive 2D subdivision.  The output t is non-increasing in each
    of a_left, a_right (positive day weights make the blend adj monotone
    non-decreasing in each a; the application chain is non-increasing in
    adj), so over a cell [l1,h1]x[l2,h2] the attainable outputs are exactly
    [t(h1,h2), t(l1,l2)] (float32 step granularity is far below one cent at
    realistic magnitudes, so no integer in between is skipped); a cell
    survives iff every constraint's observed t lies in that range.  The test
    is exact -- a cell containing a true solution always survives, so the
    returned bounding box always contains the truth; refinement stops at
    `resolution` (degrees) or `max_cells`.
    """

    def cell_ok(l1: float, h1: float, l2: float, h2: float) -> bool:
        for dl, dr, dt, v, t_obs in constraints:
            t_min = _blend_out_ext(v, h1, h2, dl, dr, dt)
            t_max = _blend_out_ext(v, l1, l2, dl, dr, dt)
            if not (t_min <= t_obs <= t_max):
                return False
        return True

    cells: List[Tuple[float, float, float, float]] = []
    if cell_ok(left_box.lo, left_box.hi, right_box.lo, right_box.hi):
        cells.append((left_box.lo, left_box.hi, right_box.lo, right_box.hi))

    while cells:
        # Refined enough, or budget exhausted?
        widest = max(max(c[1] - c[0], c[3] - c[2]) for c in cells)
        if widest <= resolution or len(cells) >= max_cells:
            break
        nxt: List[Tuple[float, float, float, float]] = []
        for l1, h1, l2, h2 in cells:
            if max(h1 - l1, h2 - l2) <= resolution:
                nxt.append((l1, h1, l2, h2))
                continue
            if h1 - l1 >= h2 - l2:
                mid = f32((l1 + h1) / 2.0)
                if not (l1 < mid < h1):
                    nxt.append((l1, h1, l2, h2))
                    continue
                halves = [(l1, mid, l2, h2), (mid, h1, l2, h2)]
            else:
                mid = f32((l2 + h2) / 2.0)
                if not (l2 < mid < h2):
                    nxt.append((l1, h1, l2, h2))
                    continue
                halves = [(l1, h1, l2, mid), (l1, h1, mid, h2)]
            for cell in halves:
                if cell_ok(*cell):
                    nxt.append(cell)
        if not nxt:
            cells = []
            break
        cells = nxt

    if not cells:
        return FractionRegion(False, None, None, 0)

    def exact_at(al: float, ar: float) -> bool:
        for dl, dr, dt, v, t_obs in constraints:
            if _blend_out_ext(v, al, ar, dl, dr, dt) != t_obs:
                return False
        return True

    witness: Optional[Tuple[float, float]] = None
    for l1, h1, l2, h2 in cells:
        for al, ar in (
            (f32((l1 + h1) / 2.0), f32((l2 + h2) / 2.0)),
            (l1, l2),
            (l1, h2),
            (h1, l2),
            (h1, h2),
        ):
            if exact_at(al, ar):
                witness = (al, ar)
                break
        if witness is not None:
            break

    return FractionRegion(
        True,
        Interval(min(c[0] for c in cells), max(c[1] for c in cells)),
        Interval(min(c[2] for c in cells), max(c[3] for c in cells)),
        len(cells),
        witness,
    )
