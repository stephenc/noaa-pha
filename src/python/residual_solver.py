"""Exact residual solver: decompose QCF-QCU into TOB regimes + PHA segments.

The model, per month (y, m) with data:

    t(y,m)   = qcu(y,m) + tobo(y,m)          # integer cents in the file
    qcf(y,m) = fp32.pha_qcf(t, S_seg)        # float32 NINT chain

TOB application is value-dependent float32 (same functional form as PHA), so
tobo(y, m) is NOT constant per calendar month: at half-cent knife edges the
offset differs between years.  This solver therefore consumes ONLY
Basis.code_offsets_by_ym -- the exact per-(y, m) offsets observed from the
matrix run against the hard-linked real data -- and never per-month
consensus constants.  S_seg is a float32 per-PHA-segment sum recovered as an
INTERVAL by intersecting fp32.pha_sum_interval(t, qcf) constraints -- never
a tolerance comparison.  A systematic single-month residual disagreement is
compiler/coordinate divergence evidence; the knife-edge retry below is a
diagnostic only and fires rarely because TOBMain mirrors the genuine v3
arithmetic.

Cost order (lexicographic): hard_shorts (PHA segments with
< HARD_SHORT_CONSTRAINTS constraint months -- physically unproducible by
PHA, so they outrank deviants), deviants, tob_changes, pha_changepoints,
soft_shorts (audit: segments < 18 QCF-visible months, regimes < 12 months).
"""

from __future__ import annotations

import bisect
import heapq
import itertools
from dataclasses import dataclass, field
from functools import lru_cache
from typing import Callable, Dict, Iterable, List, Optional, Sequence, Tuple

import fp32
from fp32 import Interval

MISSING = -9999
MIN_SEG_VISIBLE = 18  # PHA pha.adjust.min-length, counted on visible months
MIN_REGIME_MONTHS = 12  # soft prior: TOB regimes shorter than this are rare
HARD_SHORT_CONSTRAINTS = 3  # segments with fewer constraints: vintage-skew tell
MAX_DEVIANTS_PER_SEGMENT = 2  # cap on vintage-skew conversions per segment
FLIP_SPAN = 2  # regimes closing within <= this many months price as deviants
# --- basis-error excursion pricing ------------------------------------------
# A basis whose nint(100*a) rounded opposite to NOAA's for ONE (code,
# calendar-month) cell shifts that month's offsets by +-1 cent every year.
# The search could then invent a sub-12-month "excursion" regime whose code
# differs from the era code ONLY in that month by ~1 cent, absorbing one bad
# month per excursion (a <12-month regime meets each calendar month at most
# once).  Charging such SUSPICIOUS excursions SHORT_EXCURSION_CHARGE
# deviant-equivalents makes them strictly lose to the honest deviant they
# absorb, while genuine trial regimes -- consecutive-calendar-month evidence,
# amplitudes above rounding scale, or a PHR-documented change date -- are
# exempt.
SHORT_EXCURSION_CHARGE = 2  # deviant-equivalents added to a suspicious excursion
SMALL_EXCURSION_CENTS = 2  # max |offset diff| for an excursion to be suspicious
KE_MIN_REPEATS = 2  # same-month deviants needed for a solution-driven KE patch
# Branch&bound safety valve.  Sized from USC00024977: a 1029-month station
# with 5 genuine regimes needed ~200k pops (10 s) to reach a clean exact --
# 60k silently returned no-solution for it.
SEARCH_POP_CAP = 500_000
# Max pareto entries per (position, code); componentwise dominance lets
# incomparable tuples accumulate, so the frontier is bounded with
# lexicographic eviction.
PARETO_CAP = 24
# Cumulative pop budget across a station's attempts (including diagnostic
# sweeps); once a best solution exists and the budget is spent, further
# searches are skipped.
POP_BUDGET = 2_000_000
NO_TOB_CODE = "24HR"

# Evidence-schema version.  Bumped whenever solver changes could alter the
# semantics of the evidence block attached to a Solution (constraint runs,
# ambiguity sets, boundary kinds, feasible-day sets).  Written into every
# hints file's provenance; the hint loader warns on mismatch (staleness).
SOLVER_EVIDENCE_VERSION = 1

# Tie-break preference: the empirically most common obs-time codes first,
# then everything else stable.
CODE_PREF = ["07HR", "17HR", "08HR", "18HR", "24HR", "06HR", "16HR"]


def _code_rank(code: str) -> Tuple[int, str]:
    try:
        return (CODE_PREF.index(code), code)
    except ValueError:
        return (len(CODE_PREF), code)


def _mi(y: int, m: int) -> int:
    return y * 12 + (m - 1)


def _ym(mi: int) -> Tuple[int, int]:
    return (mi // 12, mi % 12 + 1)


@lru_cache(maxsize=2_000_000)
def _psi(t: int, q: int) -> Optional[Interval]:
    return fp32.pha_sum_interval(t, q)


def _isect(a: Optional[Interval], b: Optional[Interval]) -> Optional[Interval]:
    if a is None or b is None:
        return None
    lo = a.lo if a.lo >= b.lo else b.lo
    hi = a.hi if a.hi <= b.hi else b.hi
    if lo > hi:
        return None
    return Interval(lo, hi)


ZERO = Interval(0.0, 0.0)
FULL = Interval(-fp32.FLT_MAX, fp32.FLT_MAX)


# ---------------------------------------------------------------------------
# Output structures
# ---------------------------------------------------------------------------


@dataclass
class SegmentOut:
    begin: Tuple[int, int]  # (y, m) first month covered (calendar)
    end: Tuple[int, int]  # (y, m) last month covered
    s_lo: float
    s_hi: float
    n_constraints: int
    visible: int  # months QCF non-missing or X-flagged in [begin, end]

    def to_dict(self) -> dict:
        return {
            "begin": list(self.begin),
            "end": list(self.end),
            "s_interval": [self.s_lo, self.s_hi],
            "n_constraints": self.n_constraints,
            "visible": self.visible,
        }


@dataclass
class RegimeOut:
    begin: Tuple[int, int, int]  # (y, m, d)
    end: Optional[Tuple[int, int, int]]
    code: str
    blend_day: Optional[int] = None  # set when begin is a resolved mid-month day

    def to_dict(self) -> dict:
        return {
            "begin": list(self.begin),
            "end": list(self.end) if self.end else None,
            "code": self.code,
            "blend_day": self.blend_day,
        }


@dataclass
class Solution:
    station_id: str
    kind: str  # "pha-only" | "tob"
    coord_index: Optional[int]
    regimes: List[RegimeOut]
    segments: List[SegmentOut]
    deviants: List[Tuple[Tuple[int, int], str]]
    knife_edges: List[Tuple[str, int, int]]  # (code, month, delta)
    audits: List[str]
    cost: Tuple[int, int, int, int, int, int]
    exact: bool
    stats: Dict[str, object] = field(default_factory=dict)
    evidence: Optional[dict] = None  # attached by the public solve wrappers
    # Phase 2 (§9.1) laundering guard: begin-date tuples of regimes whose
    # reading was a POLICY adoption (a vintage hint tipped an equal-rank tie).
    # Such regimes export as class "residual-proven-hinted" -- never adoptable
    # downstream.  Empty/None means no vintage-hint influence on structure.
    hint_influenced: Optional[set] = None

    def to_dict(self) -> dict:
        d = {
            "station_id": self.station_id,
            "kind": self.kind,
            "coord_index": self.coord_index,
            "regimes": [r.to_dict() for r in self.regimes],
            "segments": [s.to_dict() for s in self.segments],
            "deviants": [[list(ym), why] for ym, why in self.deviants],
            "knife_edges": [list(k) for k in self.knife_edges],
            "audits": self.audits,
            "cost": list(self.cost),
            "exact": self.exact,
            "stats": self.stats,
        }
        if self.evidence is not None:
            d["evidence"] = self.evidence
        return d


# ---------------------------------------------------------------------------
# Series preparation
# ---------------------------------------------------------------------------


@dataclass
class Series:
    sid: str
    months: List[int]  # month indices, ascending, qcu & qcf both present
    t_raw: List[int]  # qcu cents aligned to months
    q: List[int]  # qcf cents aligned to months
    qcf_only: List[int]  # month indices in qcf but not qcu (deviants upfront)
    visible: set  # month indices where qcf non-missing or X-flagged
    visible_sorted: List[int] = field(default_factory=list)  # sorted(visible)


def prepare_series(
    sid: str,
    qcu: Dict[Tuple[int, int], int],
    qcf: Dict[Tuple[int, int], int],
    qcf_flags: Optional[Dict[Tuple[int, int], str]] = None,
) -> Series:
    common = sorted(set(qcu) & set(qcf), key=lambda ym: _mi(*ym))
    months = [_mi(*ym) for ym in common]
    t_raw = [qcu[ym] for ym in common]
    q = [qcf[ym] for ym in common]
    qcf_only = sorted(_mi(*ym) for ym in set(qcf) - set(qcu))
    visible = {_mi(*ym) for ym in qcf}
    if qcf_flags:
        for ym, fl in qcf_flags.items():
            if len(fl) >= 2 and fl[1] == "X":
                visible.add(_mi(*ym))
    return Series(sid, months, t_raw, q, qcf_only, visible, sorted(visible))


def _count_visible(series: Series, mi_lo: int, mi_hi: int) -> int:
    # Bisect range count on the precomputed sorted list: this is the hottest
    # call in the solver (~45% of wall time via _dp_resegment -> short_pen
    # before the fix; 680k calls in a 20-station benchmark).  Provably
    # identical to the linear scan over series.visible.
    vs = series.visible_sorted
    if not vs and series.visible:
        # Series constructed without the precomputed list (external callers)
        vs = sorted(series.visible)
        series.visible_sorted = vs
    return bisect.bisect_right(vs, mi_hi) - bisect.bisect_left(vs, mi_lo)


# ---------------------------------------------------------------------------
# PHA-only backward walk + repairs
# ---------------------------------------------------------------------------


@dataclass
class _Seg:
    idxs: List[int]  # constraint indices (into series arrays), ascending
    iv: Interval


def _walk_backward(
    series: Series,
    tobo: Optional[Sequence[int]],
    seed_zero: bool,
) -> Optional[Tuple[List[_Seg], List[int], List[int]]]:
    """Greedy backward walk with a fixed tobo (None = zeros).

    Returns (segments oldest-first, deviant indices, flutter indices) or
    None when seed_zero is requested but even the newest month contradicts
    S=0.  Contradictions within FLUTTER_EPS are flutter, not boundaries.
    """
    n = len(series.months)
    segs: List[_Seg] = []  # built newest-first
    deviants: List[int] = []
    flutters: List[int] = []
    cur: Optional[_Seg] = None
    for i in range(n - 1, -1, -1):
        t = series.t_raw[i] + (tobo[i] if tobo is not None else 0)
        iv = _psi(t, series.q[i])
        if iv is None:
            deviants.append(i)
            continue
        if cur is None:
            seeded = _isect(iv, ZERO) if seed_zero and not segs else iv
            if seeded is None:
                if seed_zero and not segs:
                    return None  # newest month contradicts S=0 -> caller falls back
                seeded = iv
            cur = _Seg([i], seeded)
            continue
        ni = _isect(cur.iv, iv)
        if ni is not None:
            cur.iv = ni
            cur.idxs.append(i)
        else:
            gap = _flutter_gap(iv, cur.iv)
            if (
                gap is not None
                and gap < FLUTTER_EPS
                and len(cur.idxs) >= FLUTTER_MIN_SEG
            ):
                flutters.append(i)
                continue
            cur.idxs.sort()
            segs.append(cur)
            cur = _Seg([i], iv)
    if cur is not None:
        cur.idxs.sort()
        segs.append(cur)
    segs.reverse()  # oldest-first
    return segs, sorted(deviants), sorted(flutters)


def _repair_short_segments(
    series: Series,
    tobo: Optional[Sequence[int]],
    segs: List[_Seg],
    deviants: List[int],
) -> Tuple[List[_Seg], List[int], List[int]]:
    """Convert vintage-skew mini-segments into deviants.

    A PHA segment with < HARD_SHORT_CONSTRAINTS constraint months is not
    physically producible (min-length 18).  Two strategies per mini-segment,
    preferring fewer (deviants, segments):
      * MERGE: drop all its months and fuse LEFT+RIGHT into one segment;
      * DISTRIBUTE: give a maximal prefix to LEFT and a maximal suffix to
        RIGHT (a skew month adjacent to a true changepoint otherwise drags a
        genuine neighbour month into a wrong 2-constraint segment); leftover
        months become deviants.
    Conversions are capped globally at max(4, 2*n_segments); past the cap the
    multi-segment solution is kept as-is.  Returns (segs, walk_deviants,
    repair_deviants) -- repair conversions are labeled separately
    ("repair-merged") by the caller.
    """

    def seg_interval(idxs: Iterable[int]) -> Optional[Interval]:
        iv: Optional[Interval] = FULL
        for i in idxs:
            t = series.t_raw[i] + (tobo[i] if tobo is not None else 0)
            iv = _isect(iv, _psi(t, series.q[i]))
            if iv is None:
                return None
        return iv

    repair_devs: List[int] = []
    total_cap = max(4, 2 * len(segs))
    changed = True
    while changed:
        changed = False
        for k in range(len(segs)):
            if len(segs[k].idxs) >= HARD_SHORT_CONSTRAINTS:
                continue
            if len(segs[k].idxs) > MAX_DEVIANTS_PER_SEGMENT:
                continue
            if len(repair_devs) + len(segs[k].idxs) > total_cap:
                continue
            left = segs[k - 1] if k > 0 else None
            right = segs[k + 1] if k + 1 < len(segs) else None
            mini = segs[k].idxs
            candidates = []  # (n_new_devs, n_segments, segs, new_devs)

            # MERGE left+right into one segment, all mini months -> deviants
            merged_idxs: List[int] = []
            if left:
                merged_idxs += left.idxs
            if right:
                merged_idxs += right.idxs
            if merged_idxs:
                iv = seg_interval(merged_idxs)
                if iv is not None:
                    nsegs = (
                        segs[: max(k - 1, 0)]
                        + [_Seg(sorted(merged_idxs), iv)]
                        + segs[k + 2 :]
                    )
                    candidates.append((len(mini), len(nsegs), nsegs, list(mini)))

            # DISTRIBUTE: maximal prefix -> LEFT, maximal suffix -> RIGHT
            if left or right:
                pre = 0
                if left:
                    while pre < len(mini):
                        if seg_interval(left.idxs + mini[: pre + 1]) is None:
                            break
                        pre += 1
                suf = 0
                if right:
                    while suf < len(mini) - pre:
                        if (
                            seg_interval(mini[len(mini) - suf - 1 :] + right.idxs)
                            is None
                        ):
                            break
                        suf += 1
                # pre/suf may both be 0: the pure-drop case (mini removed,
                # neighbours stay separate, all mini months -> deviants)
                leftovers = mini[pre : len(mini) - suf]
                nsegs = list(segs[:k]) + list(segs[k + 1 :])
                pos = k
                if left and pre:
                    li = sorted(left.idxs + mini[:pre])
                    nsegs[k - 1] = _Seg(li, seg_interval(li))
                if right and suf:
                    ri = sorted(mini[len(mini) - suf :] + right.idxs)
                    nsegs[pos] = _Seg(ri, seg_interval(ri))
                candidates.append((len(leftovers), len(nsegs), nsegs, list(leftovers)))

            if not candidates:
                continue
            candidates.sort(key=lambda c: (c[0], c[1]))
            n_dev, _n_seg, nsegs, new_devs = candidates[0]
            if len(repair_devs) + n_dev > total_cap:
                continue
            segs = nsegs
            repair_devs = sorted(repair_devs + new_devs)
            changed = True
            break
    return segs, sorted(deviants), repair_devs


def _dp_resegment(
    series: Series,
    tobo: Optional[Sequence[int]],
    seed_zero: bool,
    exclude: Optional[set] = None,
) -> Optional[Tuple[List[_Seg], List[int]]]:
    """DP over break positions minimizing (violations, segments).

    Only called when greedy leaves soft-short segments.  `exclude` holds
    constraint indices already ruled deviant by the walk/repair -- the DP
    runs on the remaining (active) indices only, so a deviant month is never
    simultaneously priced as a deviant AND constrained inside a segment.
    """
    n_all = len(series.months)
    if n_all == 0:
        return [], []
    exclude = exclude or set()
    active = [i for i in range(n_all) if i not in exclude]
    n = len(active)
    if n == 0:
        return [], []

    def _extent(p: int) -> int:
        """Largest position pj (into `active`) feasible from position p."""
        iv: Optional[Interval] = FULL
        pj = p
        while pj < n:
            i = active[pj]
            t = series.t_raw[i] + (tobo[i] if tobo is not None else 0)
            ni = _isect(iv, _psi(t, series.q[i]))
            if ni is None:
                return pj - 1
            iv = ni
            pj += 1
        return n - 1

    ext = [_extent(p) for p in range(n)]

    def short_pen(p: int, pj: int) -> int:
        lo, hi = series.months[active[p]], series.months[active[pj]]
        return 1 if _count_visible(series, lo, hi) < MIN_SEG_VISIBLE else 0

    INF = (10**9, 10**9)
    # f[p] = best (violations, segments) covering active[p:]
    f: List[Tuple[int, int]] = [INF] * (n + 1)
    nxt: List[int] = [-1] * (n + 1)
    f[n] = (0, 0)
    for p in range(n - 1, -1, -1):
        for pj in range(p, ext[p] + 1):
            cand = (f[pj + 1][0] + short_pen(p, pj), f[pj + 1][1] + 1)
            if cand < f[p]:
                f[p] = cand
                nxt[p] = pj + 1
    if f[0] == INF:
        return None
    # rebuild segments
    segs: List[_Seg] = []
    p = 0
    while p < n:
        pj = nxt[p]
        idxs = [active[x] for x in range(p, pj)]
        iv: Optional[Interval] = FULL
        for i in idxs:
            t = series.t_raw[i] + (tobo[i] if tobo is not None else 0)
            iv = _isect(iv, _psi(t, series.q[i]))
        assert iv is not None
        segs.append(_Seg(idxs, iv))
        p = pj
    if seed_zero and segs:
        seeded = _isect(segs[-1].iv, ZERO)
        if seeded is None:
            return None
        segs[-1].iv = seeded
    return segs, []


def _finalize_segments(series: Series, segs: List[_Seg]) -> List[SegmentOut]:
    out: List[SegmentOut] = []
    for k, s in enumerate(segs):
        first_i, last_i = s.idxs[0], s.idxs[-1]
        # calendar coverage extends midway to neighbours; use constraint span
        begin_mi = series.months[first_i]
        end_mi = series.months[last_i]
        if k == 0:
            begin_mi = min(begin_mi, series.months[0])
        if k == len(segs) - 1:
            end_mi = max(end_mi, series.months[-1])
        out.append(
            SegmentOut(
                begin=_ym(begin_mi),
                end=_ym(end_mi),
                s_lo=s.iv.lo,
                s_hi=s.iv.hi,
                n_constraints=len(s.idxs),
                visible=_count_visible(series, begin_mi, end_mi),
            )
        )
    return out


def _cost_of(
    series: Series,
    segs: List[_Seg],
    deviants: List[int],
    tob_changes: int,
    short_regimes: int,
    flutter: int = 0,
) -> Tuple[int, int, int, int, int, int]:
    """Cost order (hard, dev, tob, cp, flutter, soft): flutter ranks BELOW
    structural additions so one reproducibility-floor month never buys a
    fictitious excursion (1 flutter < 1 tob_change), while true deviants
    keep outranking tob_changes as before."""
    hard = sum(1 for s in segs if _gap_hard(series, s.idxs))
    soft = 0
    for k, s in enumerate(segs):
        lo = series.months[s.idxs[0]]
        hi = series.months[s.idxs[-1]]
        if _count_visible(series, lo, hi) < MIN_SEG_VISIBLE and len(segs) > 1:
            soft += 1
    cps = max(0, len(segs) - 1)
    return (hard, len(deviants), tob_changes, cps, flutter, soft + short_regimes)


def solve_pha_only(
    qcu: Dict[Tuple[int, int], int],
    qcf: Dict[Tuple[int, int], int],
    qcf_flags: Optional[Dict[Tuple[int, int], str]] = None,
    sid: str = "",
) -> Solution:
    """PHA-only decomposition (non-CONUS stations; CONUS fast path).

    Single-exit wrapper: ``_solve_pha_only_impl`` has several return points;
    this wrapper attaches the pha-only evidence block (constraint runs plus a
    single 24HR pseudo-regime, see the proposal §3.2) on the way out.
    """
    series = prepare_series(sid, qcu, qcf, qcf_flags)
    sol = _solve_pha_only_impl(series, sid)
    sol.evidence = _pha_only_evidence(series, sol)
    return sol


def _solve_pha_only_impl(series: Series, sid: str) -> Solution:
    upfront = [(_ym(mi), "qcf-only-month") for mi in series.qcf_only]
    if not series.months:
        return Solution(
            sid,
            "pha-only",
            None,
            [],
            [],
            upfront,
            [],
            ["no-common-months"],
            (0, len(upfront), 0, 0, 0, 0),
            exact=not upfront,
            stats={"n_months": 0},
        )

    attempts = []
    for seed_zero in (True, False):
        walked = _walk_backward(series, None, seed_zero)
        if walked is None:
            continue
        wsegs, wdevs, wflut = walked
        segs, walk_devs, repair_devs = _repair_short_segments(
            series, None, wsegs, wdevs
        )
        all_devs = sorted(set(walk_devs) | set(repair_devs))
        cost = _cost_of(series, segs, all_devs, 0, 0, flutter=len(wflut))
        if cost[5] > 0:  # soft shorts -> try DP resegmentation
            dp = _dp_resegment(
                series, None, seed_zero, exclude=set(all_devs) | set(wflut)
            )
            if dp is not None:
                dsegs, _ = dp
                dcost = _cost_of(series, dsegs, all_devs, 0, 0, flutter=len(wflut))
                if dcost < cost:
                    segs, cost = dsegs, dcost
        attempts.append((cost, seed_zero, segs, walk_devs, repair_devs, wflut))
    if not attempts:
        return Solution(
            sid,
            "pha-only",
            None,
            [],
            [],
            upfront,
            [],
            ["walk-infeasible"],
            (0, len(upfront), 0, 0, 0, 0),
            False,
            stats={"n_months": len(series.months)},
        )

    # Preference: among "exactish" attempts (no hard shorts, no deviants)
    # prefer the SEEDED walk regardless of changepoint count -- an unseeded
    # walk can merge the two newest true segments when their union happens to
    # be feasible, silently losing the S=0 anchor.  Otherwise best cost.
    def _rank(a):
        cost, seed_zero = a[0], a[1]
        exactish = cost[0] == 0 and cost[1] == 0
        return (0 if exactish else 1, 0 if (exactish and seed_zero) else 1, cost)

    attempts.sort(key=_rank)
    cost, seed_zero, segs, walk_dev_idx, repair_dev_idx, flut_idx = attempts[0]
    audits = [] if seed_zero else ["tail-anchor-fallback"]
    for s in segs:
        lo = series.months[s.idxs[0]]
        hi = series.months[s.idxs[-1]]
        if _count_visible(series, lo, hi) < MIN_SEG_VISIBLE and len(segs) > 1:
            audits.append(f"short-segment@{_ym(lo)}")
    deviants = (
        upfront
        + [(_ym(series.months[i]), "no-single-S") for i in walk_dev_idx]
        + [(_ym(series.months[i]), "repair-merged") for i in repair_dev_idx]
        + [(_ym(series.months[i]), "flutter") for i in flut_idx]
    )
    # Hard-short segments (< HARD_SHORT_CONSTRAINTS constraint months) rank
    # poorly in the search cost but do NOT break exactness: PHA's min-length
    # counts process months, and deletions can leave a legitimate segment
    # with very few QCF-visible months.  Audit-flag them.
    for s in segs:
        if len(s.idxs) < HARD_SHORT_CONSTRAINTS:
            audits.append(f"hard-short-segment@{_ym(series.months[s.idxs[0]])}")
    exact = not upfront and not walk_dev_idx and not repair_dev_idx
    return Solution(
        sid,
        "pha-only",
        None,
        [],
        _finalize_segments(series, segs),
        deviants,
        [],
        audits,
        cost,
        exact,
        stats={
            "n_months": len(series.months),
            "n_segments": len(segs),
            "seeded": seed_zero,
        },
    )


# ---------------------------------------------------------------------------
# TOB station solve: tail anchor, branch & bound, fallbacks, knife edges
# ---------------------------------------------------------------------------


def _tobo_arrays(series: Series, basis) -> Dict[str, Optional[List[Optional[int]]]]:
    """Per-code tobo aligned to series constraint months (None = unknown)."""
    out: Dict[str, Optional[List[Optional[int]]]] = {}
    for code, by_ym in basis.code_offsets_by_ym.items():
        arr: List[Optional[int]] = []
        by_mi = {_mi(y, m): off for (y, m), off in by_ym.items()}
        for mi in series.months:
            arr.append(by_mi.get(mi))
        out[code] = arr
    return out


# ---------------------------------------------------------------------------
# Evidence capture (see the proposal §4).  Pure read-out of what the solve
# established -- no new search, no tolerances.  Class is NOT computed here; it
# depends on post-relabel deviants and is finalized in tob_hints (§4.4).
# ---------------------------------------------------------------------------


def _runs_from_mis(mis: Sequence[int]) -> List[List[List[int]]]:
    """Group ascending month indices into maximal calendar-contiguous runs.

    Returns ``[[ [y,m], [y,m] ], ...]``; a gap (missing month, or an excluded
    deviant/flutter/blend month left out of *mis*) breaks a run -- holes stay
    visible, so a hole inside a regime is never "evidence-backed"."""
    runs: List[List[List[int]]] = []
    prev: Optional[int] = None
    start: Optional[int] = None
    for mi in mis:
        if prev is None:
            start = prev = mi
        elif mi == prev + 1:
            prev = mi
        else:
            y0, m0 = _ym(start)
            y1, m1 = _ym(prev)
            runs.append([[y0, m0], [y1, m1]])
            start = prev = mi
    if start is not None:
        y0, m0 = _ym(start)
        y1, m1 = _ym(prev)
        runs.append([[y0, m0], [y1, m1]])
    return runs


def _deviant_flutter_mis(sol: Solution) -> Tuple[set, set]:
    """(deviant month indices, flutter month indices) from a solution.

    Deviant = the demoting labels (unexplained / blend-unresolved for TOB;
    no-single-S / repair-merged for pha-only).  ``qcf-only-month`` months are
    never constraint months (qcu absent), so they do not appear here."""
    demoting = {"unexplained", "blend-unresolved", "no-single-S", "repair-merged"}
    dev: set = set()
    flut: set = set()
    for ym, why in sol.deviants:
        mi = _mi(ym[0], ym[1])
        if why == "flutter":
            flut.add(mi)
        elif why in demoting:
            dev.add(mi)
    return dev, flut


def _regime_begin_mi(reg: RegimeOut) -> int:
    return _mi(reg.begin[0], reg.begin[1])


def _blend_day_source(sol: Solution, bi: int) -> Tuple[str, Optional[List[int]]]:
    """(day_source, feasible_days) for a resolved blend at constraint index bi."""
    feas_map = sol.stats.get("blend_days_feasible") or {}
    feas = feas_map.get(bi)
    if feas is None:
        # JSON round-trips int keys to strings; tolerate both.
        feas = feas_map.get(str(bi))
    if feas is not None:
        return "search-blend", [int(d) for d in feas]
    for a in sol.audits:
        if "magnitude-refined" in a:
            return "magnitude-refined", None
    return "two-blend-repair", None


def _solution_evidence(
    series: Series,
    tobos: Dict[str, Optional[List[Optional[int]]]],
    sol: Solution,
) -> dict:
    """Evidence block for a TOB solution: constraint-month runs, per-regime
    constrained runs / coverage, offset-identity ambiguity sets (deviant /
    flutter / blend indices excluded), boundary kinds, and blend feasible-day
    sets.  Entries carry their own begin date -- consumers align by begin, not
    list position (§4.3)."""
    months = series.months
    n = len(months)
    pos_of = {mi: i for i, mi in enumerate(months)}
    dev_mis, flut_mis = _deviant_flutter_mis(sol)

    regs = sol.regimes
    begins = [_regime_begin_mi(r) for r in regs]

    out_regimes: List[dict] = []
    prev_last_mi: Optional[int] = None
    for k, reg in enumerate(regs):
        begin_mi = begins[k]
        next_mi = begins[k + 1] if k + 1 < len(regs) else None
        blend_mi = begin_mi if reg.blend_day else None
        span_pos = [
            i
            for i in range(n)
            if months[i] >= begin_mi and (next_mi is None or months[i] < next_mi)
        ]
        idxs = [
            i
            for i in span_pos
            if months[i] not in dev_mis
            and months[i] not in flut_mis
            and months[i] != blend_mi
        ]
        idx_mis = [months[i] for i in idxs]
        chosen = reg.code
        chosen_arr = tobos.get(chosen)
        if idxs and chosen_arr is not None:
            amb = [
                c
                for c in sorted(tobos, key=_code_rank)
                if tobos[c] is not None
                and all(
                    tobos[c][i] is not None and tobos[c][i] == chosen_arr[i]
                    for i in idxs
                )
            ]
        else:
            amb = []
        if chosen not in amb:
            amb = [chosen] + amb

        # Boundary kind for this begin.
        if k == 0:
            boundary = {
                "kind": "record-start",
                "gap_months_before": None,
            }
        elif prev_last_mi is not None and begin_mi == prev_last_mi + 1:
            boundary = {"kind": "constrained", "gap_months_before": None}
        elif prev_last_mi is not None:
            boundary = {
                "kind": "gap",
                "gap_months_before": begin_mi - prev_last_mi - 1,
            }
        else:
            boundary = {"kind": "gap", "gap_months_before": None}

        if reg.blend_day:
            src, feas = _blend_day_source(sol, pos_of.get(begin_mi, -1))
            boundary["day_resolved"] = True
            boundary["day_source"] = src
            boundary["feasible_days"] = feas
        else:
            boundary["day_resolved"] = False
            boundary["day_source"] = None
            boundary["feasible_days"] = None

        span_dev = sorted(
            mi for mi in dev_mis if begin_mi <= mi and (next_mi is None or mi < next_mi)
        )
        span_flut = sorted(
            mi for mi in flut_mis if begin_mi <= mi and (next_mi is None or mi < next_mi)
        )
        influenced = bool(sol.hint_influenced) and tuple(reg.begin) in sol.hint_influenced
        out_regimes.append(
            {
                "begin": list(reg.begin),
                "code": reg.code,
                "blend_day": reg.blend_day,
                "n_constrained": len(idxs),
                "constrained_runs": _runs_from_mis(idx_mis),
                "first_constrained": list(_ym(idx_mis[0])) if idx_mis else None,
                "last_constrained": list(_ym(idx_mis[-1])) if idx_mis else None,
                "ambiguous_codes": amb,
                "flutter_months": [list(_ym(mi)) for mi in span_flut],
                "deviant_months": [list(_ym(mi)) for mi in span_dev],
                "boundary": boundary,
                "hint_influenced": influenced,
            }
        )
        if idx_mis:
            prev_last_mi = idx_mis[-1]

    return {
        "solver_version": SOLVER_EVIDENCE_VERSION,
        "kind": "tob",
        "evidence_runs": _runs_from_mis(months),
        "regimes": out_regimes,
    }


def _pha_only_evidence(series: Series, sol: Solution) -> dict:
    """Evidence block for a pha-only solution: constraint runs plus a single
    24HR pseudo-regime with ambiguity set {24HR, 00HR} (§3.2)."""
    months = series.months
    evidence_runs = _runs_from_mis(months)
    if not months:
        return {
            "solver_version": SOLVER_EVIDENCE_VERSION,
            "kind": "pha-only",
            "evidence_runs": evidence_runs,
            "regimes": [],
        }
    dev_mis, flut_mis = _deviant_flutter_mis(sol)
    idx_mis = [mi for mi in months if mi not in dev_mis and mi not in flut_mis]
    span_dev = sorted(dev_mis)
    span_flut = sorted(flut_mis)
    fy, fm = _ym(months[0])
    regime = {
        "begin": [fy, fm, 1],
        "code": NO_TOB_CODE,  # 24HR: zero adjustment, proven over the span
        "blend_day": None,
        "n_constrained": len(idx_mis),
        "constrained_runs": _runs_from_mis(idx_mis),
        "first_constrained": list(_ym(idx_mis[0])) if idx_mis else None,
        "last_constrained": list(_ym(idx_mis[-1])) if idx_mis else None,
        "ambiguous_codes": ["24HR", "00HR"],
        "flutter_months": [list(_ym(mi)) for mi in span_flut],
        "deviant_months": [list(_ym(mi)) for mi in span_dev],
        "boundary": {
            "kind": "record-start",
            "gap_months_before": None,
            "day_resolved": False,
            "day_source": None,
            "feasible_days": None,
        },
    }
    return {
        "solver_version": SOLVER_EVIDENCE_VERSION,
        "kind": "pha-only",
        "evidence_runs": evidence_runs,
        "regimes": [regime],
    }


@dataclass(order=True)
class _State:
    cost: Tuple[int, int, int, int, int, int]
    order: int
    i: int = field(compare=False)  # next constraint index to consume (descending)
    code: str = field(compare=False)
    iv: Interval = field(compare=False)
    seg_idxs: Tuple[int, ...] = field(compare=False)
    regime_hi_mi: int = field(compare=False)  # newest month of current regime
    trace: object = field(compare=False)  # parent linked list


def _advance_greedy(
    series: Series,
    tobo: List[Optional[int]],
    i: int,
    iv: Interval,
    seg_idxs: List[int],
) -> Tuple[int, Interval, List[int]]:
    """Consume constraints (descending i) while a single S stays feasible."""
    while i >= 0:
        off = tobo[i]
        if off is None:
            break
        ni = _isect(iv, _psi(series.t_raw[i] + off, series.q[i]))
        if ni is None:
            break
        iv = ni
        seg_idxs.append(i)
        i -= 1
    return i, iv, seg_idxs


def _solve_with_basis(
    series: Series,
    basis,
    seed_zero: bool,
    cap: Optional[int] = None,
    no_blend: bool = False,
    hint_mis: frozenset = frozenset(),
    flutter_as_deviant: bool = False,
    hint_code_at: Optional[Dict[int, str]] = None,
) -> Optional[dict]:
    """Backward branch & bound for one coordinate basis.

    Returns a dict with the winning trace decoded, or None.  no_blend
    disables the mid-month blend branch (used for the rerun when a packaged
    solution carries blend-unresolved deviants: the search prices a blend as
    one cheap tob-change and cannot foresee resolution failure, which can
    shadow a genuine trial-regime reading, e.g. USC00049099).

    hint_code_at (Phase 2 §9.3): {constraint index -> preferred switch-target
    code}.  Because the heap orders by (cost, insertion order), enumerating
    the hinted code FIRST in branch 2 prefers it among EQUAL-cost states -- a
    tie-break in the absence of frontier eviction (under PARETO_CAP pressure
    arrival order can also change evictions, so outcomes may differ beyond
    ties).  Absent (None) => byte-identical enumeration order to before.
    """
    cap = cap or SEARCH_POP_CAP
    tobos = _tobo_arrays(series, basis)
    codes = sorted(tobos.keys(), key=_code_rank)

    def _c2_order(i: int) -> List[str]:
        if hint_code_at is None:
            return codes
        hc = hint_code_at.get(i)
        if hc is None or hc not in tobos:
            return codes
        return [hc] + [c for c in codes if c != hc]
    n = len(series.months)
    if n == 0:
        return None

    counter = itertools.count()
    heap: List[_State] = []

    def push(cost, i, code, iv, seg_idxs, regime_hi, trace):
        heapq.heappush(
            heap,
            _State(cost, next(counter), i, code, iv, tuple(seg_idxs), regime_hi, trace),
        )

    # Seed states: each code, starting at the newest constraint -- plus
    # lead-skip seeds treating the newest 1..LEAD_SKIP_MAX constraints as
    # deviants.  Without these, an isolated/corrupt record-edge month (e.g.
    # one month after a multi-decade gap) forces EVERY path through it, and
    # dodging the resulting hard-short buys strings of bogus mid-record
    # deviants instead (USC00410611's 2003-09).
    for skip in range(0, LEAD_SKIP_MAX + 1):
        pos = n - 1 - skip
        if pos < 0:
            break
        trace0 = None
        for k in range(skip):
            trace0 = ("dev", n - 1 - k, trace0)
        for code in codes:
            iv0 = _isect(ZERO, FULL) if seed_zero else FULL
            i, iv, seg = _try_start(series, tobos[code], pos, iv0)
            if iv is None:
                continue
            i, iv, seg = _advance_greedy(series, tobos[code], i, iv, seg)
            cost = (0, skip, 0, 0, 0, 0)
            trace = ("start", code, None)
            node = trace0
            # chain: start at the bottom, dev-skips above it
            stack = []
            while node is not None:
                stack.append(node[1])
                node = node[2]
            for idx in reversed(stack):
                trace = ("dev", idx, trace)
            push(cost, i, code, iv, seg, series.months[pos], trace)

    best = None
    pops = 0
    # dominance: per (i, code) keep pareto list of (cost, iv)
    seen: Dict[Tuple[int, str], List[Tuple[Tuple, Interval]]] = {}
    while heap:
        st = heapq.heappop(heap)
        pops += 1
        if pops > cap:
            return None  # cap exceeded -> caller uses fallback
        if st.i < 0:
            best = st
            break
        key = (st.i, st.code)
        dominated = False
        plist = seen.setdefault(key, [])
        for c0, iv0 in plist:
            # Componentwise dominance: costs are additive, so a state may be
            # pruned only when EVERY component is <= (lexicographic <= is
            # unsound -- it pruned states with a strictly smaller later
            # component, e.g. USC00049099's genuine trial-regime path).
            if (
                all(a <= b for a, b in zip(c0, st.cost))
                and iv0.lo <= st.iv.lo
                and iv0.hi >= st.iv.hi
            ):
                dominated = True
                break
        if dominated:
            continue
        if len(plist) >= PARETO_CAP:
            # Bounded frontier: componentwise dominance alone lets
            # incomparable cost tuples accumulate.  Evict (or reject) by
            # lexicographic cost -- best-first order means later arrivals at
            # the same (position, code) with lexicographically larger cost
            # are almost never on the optimal path.
            worst_idx = max(range(len(plist)), key=lambda k: plist[k][0])
            if plist[worst_idx][0] <= st.cost:
                continue
            plist[worst_idx] = (st.cost, st.iv)
        else:
            plist.append((st.cost, st.iv))

        i = st.i
        mi_here = series.months[i]
        h, d, tb, cp, fl, sh = st.cost
        regime_span = st.regime_hi_mi - mi_here

        # 1. PHA changepoint, same code
        off = tobos[st.code][i]
        if off is not None:
            iv_new = _psi(series.t_raw[i] + off, series.q[i])
            if iv_new is not None:
                short = 1 if len(st.seg_idxs) < HARD_SHORT_CONSTRAINTS else 0
                ni, niv, nseg = _advance_greedy(
                    series, tobos[st.code], i - 1, iv_new, [i]
                )
                push(
                    (
                        h + _gap_hard(series, st.seg_idxs),
                        d,
                        tb,
                        cp + 1,
                        fl,
                        sh + _soft(series, st.seg_idxs),
                    ),
                    ni,
                    st.code,
                    niv,
                    nseg,
                    st.regime_hi_mi,
                    ("cp", i, st.trace),
                )
        # 2. code switches.  A switch CLOSES the current regime; a closed
        # regime spanning <= FLIP_SPAN months is the vintage-skew signature
        # (a flip A->B->A around one bad month) and is priced as a
        # deviant-equivalent (d component), not merely a soft short --
        # otherwise micro-regimes lexicographically undercut honest deviants
        # and the search chases them (or explodes).  Genuine trial-hour
        # regimes of a few months survive: only <= FLIP_SPAN is penalized.
        short_reg = 1 if regime_span < MIN_REGIME_MONTHS else 0
        flip_pen = 1 if regime_span <= FLIP_SPAN else 0
        for c2 in _c2_order(i):
            if c2 == st.code:
                continue
            off2 = tobos[c2][i]
            if off2 is None:
                continue
            # Suspicious-excursion charge: a closing
            # <12-month regime that differs from the code it switches
            # against only at one tiny-amplitude calendar month prices as
            # SHORT_EXCURSION_CHARGE deviant-equivalents -- it can absorb at
            # most one knife-edge month, so it strictly loses to the honest
            # deviant unless evidence (multi-month, amplitude, PHR) exempts.
            # A PHR hint dating the boundary lifts the flip penalty and the
            # suspicious charge ONLY in the hint-upgrade search
            # (flutter_as_deviant): documentation is evidence, but lifting
            # penalties in the NORMAL search made hinted structure free
            # in-search even when it absorbed nothing (junk regimes near
            # hints on USC00201780/USW00024057).  The upgrade pass's
            # zero-deviant acceptance criterion is the guard that makes the
            # lift safe.
            hinted = flutter_as_deviant and _hint_near(
                series, i, st.regime_hi_mi, hint_mis
            )
            sus_occ = (
                0
                if hinted
                else _suspicious_short(series, tobos, st.code, c2, i, st.regime_hi_mi)
            )
            # occurrences + 1: a near-vacuous regime absorbing N knife
            # months must strictly lose to the N honest deviants it absorbs.
            sus_pen = (sus_occ + 1) if sus_occ else 0
            flip_eff = 0 if hinted else flip_pen
            # Switch-viability prune: only branch to codes that can explain a
            # run of >= SWITCH_VIABLE_RUN constraints backward from here
            # under SOME single S (cheap greedy probe).  This cuts the
            # ~27-way branch factor to the handful of codes that actually
            # fit locally; genuine short trial regimes (>= 3 months) survive.
            if not _switch_viable(series, tobos[c2], i):
                continue
            iv2 = _psi(series.t_raw[i] + off2, series.q[i])
            # 2a. switch, same PHA segment
            cont = _isect(st.iv, iv2) if iv2 else None
            if cont is not None:
                ni, niv, nseg = _advance_greedy(
                    series, tobos[c2], i - 1, cont, list(st.seg_idxs) + [i]
                )
                push(
                    (h, d + flip_eff + sus_pen, tb + 1, cp, fl, sh + short_reg),
                    ni,
                    c2,
                    niv,
                    nseg,
                    mi_here,
                    ("switch", (i, c2, False), st.trace),
                )
            # 2b. switch + changepoint same month
            if iv2 is not None:
                ni, niv, nseg = _advance_greedy(series, tobos[c2], i - 1, iv2, [i])
                push(
                    (
                        h + _gap_hard(series, st.seg_idxs),
                        d + flip_eff + sus_pen,
                        tb + 1,
                        cp + 1,
                        fl,
                        sh + _soft(series, st.seg_idxs) + short_reg,
                    ),
                    ni,
                    c2,
                    niv,
                    nseg,
                    mi_here,
                    ("switch", (i, c2, True), st.trace),
                )
            # 2c. blend switch: boundary month i is mid-month; skip it.
            # +1 soft cost so pure month-boundary switches are strictly
            # preferred when both explain the data (keeps the search
            # deterministic and avoids post-hoc blend-resolution penalties).
            if not no_blend:
                ni, niv, nseg = _advance_greedy(
                    series, tobos[c2], i - 1, st.iv, list(st.seg_idxs)
                )
                push(
                    (h, d + flip_eff + sus_pen, tb + 1, cp, fl, sh + short_reg + 1),
                    ni,
                    c2,
                    niv,
                    nseg,
                    mi_here,
                    ("blend", (i, c2, st.code), st.trace),
                )
        # 3. deviant skip -- or FLUTTER skip when the month's own S-interval
        # misses the running intersection by < FLUTTER_EPS (reproducibility
        # floor, not structure; see FLUTTER_EPS).
        ni, niv, nseg = _advance_greedy(
            series, tobos[st.code], i - 1, st.iv, list(st.seg_idxs)
        )
        off_cur = tobos[st.code][i]
        gap = None
        if off_cur is not None and len(st.seg_idxs) >= FLUTTER_MIN_SEG:
            gap = _flutter_gap(_psi(series.t_raw[i] + off_cur, series.q[i]), st.iv)
        if gap is not None and gap < FLUTTER_EPS:
            # hint-upgrade mode (flutter_as_deviant): price the flutter into
            # the deviant component so a hinted, fully bit-exact alternative
            # reading (e.g. a PHR-documented tiny excursion) can out-rank it.
            fcost = (
                (h, d + 1, tb, cp, fl, sh)
                if flutter_as_deviant
                else (h, d, tb, cp, fl + 1, sh)
            )
            push(
                fcost,
                ni,
                st.code,
                niv,
                nseg,
                st.regime_hi_mi,
                ("flut", i, st.trace),
            )
        else:
            push(
                (h, d + 1, tb, cp, fl, sh),
                ni,
                st.code,
                niv,
                nseg,
                st.regime_hi_mi,
                ("dev", i, st.trace),
            )

    if best is None:
        return None
    return {"state": best, "pops": pops}


# A switch target must explain a single-S run this long from the branch
# point.  2 (not 3): genuine trial-hour regimes as short as two months exist
# (e.g. USC00487105's 15HR Dec-1987..Jan-1988) and must stay reachable.
SWITCH_VIABLE_RUN = 2
# Newest constraints skippable as deviants at seed time (isolated/corrupt
# record-edge months).
LEAD_SKIP_MAX = 3


def _single_s_run(series: Series, tobo: List[Optional[int]], j: int, need: int) -> bool:
    """True if constraints j, j-1, ... (need of them) admit a single S."""
    iv: Optional[Interval] = FULL
    taken = 0
    while j >= 0 and taken < need:
        off = tobo[j]
        if off is None:
            return False
        iv = _isect(iv, _psi(series.t_raw[j] + off, series.q[j]))
        if iv is None:
            return False
        taken += 1
        j -= 1
    return True


def _switch_viable(series: Series, tobo: List[Optional[int]], i: int) -> bool:
    """Is this code a credible switch target at position i?

    Credible = it explains a run of SWITCH_VIABLE_RUN consecutive
    constraints under a single S starting at i, OR starting at i-1 (the
    switch may coincide with a PHA changepoint at i, or i may be a blend
    month, so the clean run can begin one constraint earlier)."""
    need_i = min(SWITCH_VIABLE_RUN, i + 1)
    if _single_s_run(series, tobo, i, need_i):
        return True
    if i >= 1:
        need_i1 = min(SWITCH_VIABLE_RUN, i)
        return _single_s_run(series, tobo, i - 1, need_i1)
    return False


def _suspicious_short(
    series: Series,
    tobos: Dict[str, Optional[List[Optional[int]]]],
    code_y: str,
    code_flank: str,
    i: int,
    regime_hi_mi: int,
) -> int:
    """Knife-edge-absorption test for a closing regime of ANY length.

    Returns the number of diff-month OCCURRENCES (0 = not suspicious) when,
    versus the flanking code it is switching against, the regime's offsets
    differ in at most ONE distinct calendar month over its observed span,
    each by <= SMALL_EXCURSION_CENTS.  (PHR-hint exemption is applied by the
    caller via _hint_near, in the hint-upgrade search only.)  That is
    exactly what a single-(code, month) basis rounding error induces
    (demonstrated by the ifort cross-check on USC00108380/USC00272284/
    USC00397742): the excursion code is offset-identical to the era code
    except at the poisoned month, where it happens to supply the +-1 cent.

    NOT gated on regime length: the search otherwise evades a short-only
    charge by stretching the near-vacuous regime past 12 months (observed:
    a 12-month 13HR era absorbing one poisoned June).  The caller prices
    occurrences+1 deviant-equivalents so a stretch absorbing N knife months
    still strictly loses to N honest deviants.  A genuine regime between
    distinct codes exits the scan at its second distinct diff month or first
    above-rounding amplitude (seasonal vectors differ widely), so the walk
    is short in the non-suspicious case."""
    arr_y = tobos.get(code_y)
    arr_f = tobos.get(code_flank)
    if arr_y is None or arr_f is None:
        return 0
    mo_diff = set()
    occ = 0
    n = len(series.months)
    k = i
    while k < n and series.months[k] <= regime_hi_mi:
        oy, of = arr_y[k], arr_f[k]
        if oy is not None and of is not None and oy != of:
            if abs(oy - of) > SMALL_EXCURSION_CENTS:
                return 0
            mo_diff.add(series.months[k] % 12 + 1)
            if len(mo_diff) > 1:
                return 0
            occ += 1
        k += 1
    if not mo_diff:
        # offset-identical over the whole span: vacuous flip, handled by
        # canonicalization -- not priced here
        return 0
    return occ


def _hint_near(series: Series, i: int, regime_hi_mi: int, hint_mis: frozenset) -> bool:
    """Does a PHR hint date a change within +-2 months of either end of the
    closing regime's span?  A documented change exempts the regime from both
    the suspicious-excursion charge and the flip penalty -- documentation is
    the evidence the pricing otherwise demands."""
    if not hint_mis:
        return False
    for b in (series.months[i], regime_hi_mi):
        if any(abs(b - h) <= 2 for h in hint_mis):
            return True
    return False


GAP_EXEMPT_MONTHS = 12  # constraint gap that exempts a segment from hard-short

# FLUTTER: a month whose single-month S-interval misses the segment's running
# intersection by less than this (in S-space, degC) is classified 'flutter',
# costed BELOW structural additions (tob_changes/changepoints) but recorded.
# Justification: interval endpoints are float32-quantized; a sub-1e-5 miss
# (vs interval widths of ~1e-2 and ulp(S)~1.5e-8..6e-8) means NOAA's float32
# S satisfied every month under THEIR arithmetic and our reproduction of one
# endpoint differs by a final-rounding ulp -- the numerical-reproducibility
# floor (e.g. USC00456678's 1963-09: gap 5e-7).  Preferring a fictitious
# code-excursion + two changepoints over acknowledging one flutter month
# invents structure that PHR contradicts and PHA physics forbids.
FLUTTER_EPS = 1e-5
# A month may be classified flutter only against a running interval backed by
# at least this many constraints: a near-abutting miss against a 1-2-month
# (possibly corrupt) segment is no evidence of a reproducibility ulp -- it
# cascades (one skewed month can otherwise 'flutter away' an entire era).
FLUTTER_MIN_SEG = 12


def _flutter_gap(iv_month: Optional[Interval], running: Interval) -> Optional[float]:
    """Distance by which iv_month misses `running` (None if they intersect
    or iv_month is None)."""
    if iv_month is None:
        return None
    if iv_month.lo > running.hi:
        return iv_month.lo - running.hi
    if running.lo > iv_month.hi:
        return running.lo - iv_month.hi
    return None


def _gap_hard(series: Series, seg_idxs: Sequence[int]) -> int:
    """Hard-short test with gap/record-edge exemption.

    A segment with < HARD_SHORT_CONSTRAINTS constraint months is only
    "physically unproducible" when it sits in CONTINUOUS data.  At a record
    edge, or abutting a >=GAP_EXEMPT_MONTHS constraint gap, the segment's
    true extent is hidden: gap months are frequently X-flagged in QCF (PHA
    processed them, THEN deleted them -- e.g. USC00474829's 1953-07..1955-10
    are all ' X0'), so NOAA's min-length counted process months we cannot
    see.  Such segments are legitimate (USC00051772's 2-month segment before
    a 40-month hole; USC00410611's isolated 2003-09 record-edge month)."""
    if len(seg_idxs) >= HARD_SHORT_CONSTRAINTS or not seg_idxs:
        return 0
    lo_i, hi_i = min(seg_idxs), max(seg_idxs)
    if lo_i == 0 or hi_i == len(series.months) - 1:
        return 0
    if series.months[lo_i] - series.months[lo_i - 1] - 1 >= GAP_EXEMPT_MONTHS:
        return 0
    if series.months[hi_i + 1] - series.months[hi_i] - 1 >= GAP_EXEMPT_MONTHS:
        return 0
    return 1


def _soft(series: Series, seg_idxs: Sequence[int]) -> int:
    if not seg_idxs:
        return 0
    lo = series.months[min(seg_idxs)]
    hi = series.months[max(seg_idxs)]
    return 1 if _count_visible(series, lo, hi) < MIN_SEG_VISIBLE else 0


def _try_start(
    series: Series,
    tobo: List[Optional[int]],
    i: int,
    iv0: Interval,
) -> Tuple[int, Optional[Interval], List[int]]:
    off = tobo[i]
    if off is None:
        return i, None, []
    iv = _isect(iv0, _psi(series.t_raw[i] + off, series.q[i]))
    if iv is None:
        return i, None, []
    return i - 1, iv, [i]


def _decode_trace(
    series: Series,
    basis,
    state: _State,
) -> Tuple[
    Optional[str],
    List[Tuple[int, str, Optional[str], bool]],
    List[Tuple[int, str, str]],
    List[int],
    List[int],
]:
    """Walk parent links back to the start event.

    Returns (start_code, switches, blends, deviant_idxs, cp_idxs) where
    switches are (constraint_idx, older_code, None, with_cp) and blends are
    (constraint_idx, older_code, newer_code).  start_code is the code of the
    NEWEST regime (the walk began at the record tail).
    """
    switches: List[Tuple[int, str, Optional[str], bool]] = []
    blends: List[Tuple[int, str, str]] = []
    deviants: List[int] = []
    flutters: List[int] = []
    cps: List[int] = []
    node = state.trace
    start_code = None
    while node is not None:
        kind = node[0]
        if kind == "start":
            start_code = node[1]
            node = node[2]
        elif kind == "cp":
            cps.append(node[1])
            node = node[2]
        elif kind == "switch":
            i, c2, with_cp = node[1]
            switches.append((i, c2, None, with_cp))
            if with_cp:
                cps.append(i)
            node = node[2]
        elif kind == "blend":
            i, c2, c_new = node[1]
            blends.append((i, c2, c_new))
            node = node[2]
        elif kind == "dev":
            deviants.append(node[1])
            node = node[2]
        elif kind == "flut":
            flutters.append(node[1])
            node = node[2]
        else:  # pragma: no cover
            raise AssertionError(kind)
    return start_code, switches, blends, deviants, flutters, cps


def _segments_from_pairs(
    series: Series,
    tobo_of: Callable[[int], Optional[int]],
    dev_set: set,
    blend_set: set,
    seed_zero: bool = True,
) -> Tuple[List[_Seg], List[int], List[int]]:
    """Re-run the greedy walk forward-in-reverse with a KNOWN code timeline
    to produce the definitive segment list (deterministic, same result as
    the search's winning path).  seed_zero must mirror the winning search's
    flag: a tail-deletion solution must NOT have the newest
    observed segment re-anchored to S=0 here.  Contradictions within
    FLUTTER_EPS are classified flutter (skipped), mirroring the search.
    Returns (segments, extra_deviants, extra_flutter)."""
    segs: List[_Seg] = []
    cur: Optional[_Seg] = None
    extra_devs: List[int] = []
    extra_flut: List[int] = []
    for i in range(len(series.months) - 1, -1, -1):
        if i in dev_set or i in blend_set:
            continue
        off = tobo_of(i)
        if off is None:
            extra_devs.append(i)
            continue
        iv = _psi(series.t_raw[i] + off, series.q[i])
        if iv is None:
            extra_devs.append(i)
            continue
        if cur is None:
            seeded = _isect(iv, ZERO) if (seed_zero and not segs) else iv
            cur = _Seg([i], seeded if seeded is not None else iv)
            continue
        ni = _isect(cur.iv, iv)
        if ni is not None:
            cur.iv = ni
            cur.idxs.append(i)
        else:
            gap = _flutter_gap(iv, cur.iv)
            if (
                gap is not None
                and gap < FLUTTER_EPS
                and len(cur.idxs) >= FLUTTER_MIN_SEG
            ):
                extra_flut.append(i)
                continue
            cur.idxs.sort()
            segs.append(cur)
            cur = _Seg([i], iv)
    if cur is not None:
        cur.idxs.sort()
        segs.append(cur)
    segs.reverse()
    return segs, extra_devs, extra_flut


def _systematic_flutter(sol: "Solution") -> bool:
    """>=3 flutter/unexplained deviants sharing one calendar month: that is
    not random reproducibility noise -- it is the knife-edge signature (our
    basis rounded one (code, month) cell opposite to NOAA's), and the
    knife-edge machinery must get a chance even though the solution may be
    nominally 'clean but fluttery'."""
    from collections import Counter

    months = Counter(
        m for (_y, m), why in sol.deviants if why in ("unexplained", "flutter")
    )
    return bool(months) and months.most_common(1)[0][1] >= 3


def _hinted_deviant(sol: "Solution", hint_mis: frozenset) -> bool:
    """Any flutter/unexplained deviant within +-2 months of a PHR hint date?
    Such a solution must not end the search early: a documented reading
    that is fully bit-exact may exist (hint-upgrade pass)."""
    if not hint_mis:
        return False
    return any(
        why in ("flutter", "unexplained")
        and any(abs(_mi(y, m) - hmi) <= 2 for hmi in hint_mis)
        for (y, m), why in sol.deviants
    )


def _clean(sol: "Solution") -> bool:
    """Structurally sound exactness: no deviants, no hard-short segments, no
    soft-short audits.  Only this grade of solution may end the search
    early -- nominally-exact but fragmented/alternating readings (e.g.
    seasonal 24HR flips absorbing a basis error into per-winter segments)
    must keep searching."""
    return sol.exact and sol.cost[0] == 0 and sol.cost[5] == 0


def _knife_edge_prescan(series: Series, basis) -> Optional[Tuple[int, str, int, int]]:
    """Cheap tail-anchor scan for the knife-edge signature.

    Under S=0 (the newest PHA segment) R == tobo exactly.  A basis whose
    nint(100*a) rounded opposite to NOAA's for ONE (code, calendar month)
    shows up as a constant +-1 delta in exactly that month, every year.
    Returns (suffix_len, code, month, delta) for the longest such suffix
    (>= 24 months, exactly one nonzero month), or None.
    """
    n = len(series.months)
    if n == 0:
        return None
    R = [series.q[i] - series.t_raw[i] for i in range(n)]
    tobos = _tobo_arrays(series, basis)
    best: Optional[Tuple[int, str, int, int]] = None
    for code, arr in tobos.items():
        per_mo: Dict[int, int] = {}
        length = 0
        for i in range(n - 1, -1, -1):
            if arr[i] is None:
                break
            delta = R[i] - arr[i]
            mo = series.months[i] % 12 + 1
            if mo in per_mo:
                if per_mo[mo] != delta:
                    break
            else:
                if abs(delta) > 1:
                    break
                per_mo[mo] = delta
                if sum(1 for v in per_mo.values() if v != 0) > 1:
                    del per_mo[mo]
                    break
            length += 1
        nonzero = [m for m, v in per_mo.items() if v != 0]
        if len(nonzero) == 1 and length >= 24:
            if best is None or length > best[0]:
                best = (length, code, nonzero[0], per_mo[nonzero[0]])
    return best


def _ke_candidates_from_solution(
    series: Series, sol: "Solution", basis
) -> List[Tuple[str, int, int]]:
    """Mid-record knife-edge candidates derived from a solved reading.

    _knife_edge_prescan only sees the S=0 tail: its backward scan requires
    R == tobo exactly, so it dies at the first PHA changepoint (or code
    change) walking back from the record end.  A poisoned (code, month) cell
    deeper than the newest segment is invisible to it -- the root cause of
    the prescan missing the ifort cross-check cases, whose bad months sat
    decades inside the record.

    This generalization reads the signature off the best PLAIN solution
    instead: >= KE_MIN_REPEATS unexplained/flutter deviants sharing one
    calendar month, all under the SAME active code, all reconciled by the
    SAME +-1 cent shift against their reading-consistent prediction.
    Returns (code, month, delta) patch candidates, largest cluster first.

    WORKFLOW (patched attempts are DIAGNOSTIC ONLY -- they never win):
    1. Detection: a patched attempt that would be fully exact marks the
       cell as a compiler-divergence candidate on the plain winner
       ('compiler-divergence-candidate:<code>:m<month>:<+/-1>:<n_months>'
       audit + knife_edges entry).
    2. Gate-row derivation: instrument the cell's trig inputs and ulp-flip
       sweep against the candidate values (USC00222099's April cell proved
       SVML-expressible and became llvm-exact gate row 5).
    3. Not expressible: five cells (USC00402589, USC00448129 current;
       USC00243195, USC00422418, USC00461677 vintage) survived exhaustive
       +-1-ulp sweeps and full SVML-delta application with zero movement --
       ifort CODE GENERATION divergence (FMA/reassociation in the a_table
       arithmetic), not math-library.  Evidence: work/gate_scratch2/.
       These remain documented divergences; the emitted pipeline stays
       self-verifying because the plain reading is what gets emitted."""
    if not sol.deviants or not sol.regimes:
        return []
    idx_of = {m: i for i, m in enumerate(series.months)}
    bounds = sorted((_mi(r.begin[0], r.begin[1]), r.code) for r in sol.regimes)

    def active_code(mi_month: int) -> Optional[str]:
        c = None
        for bmi, bc in bounds:
            if bmi <= mi_month:
                c = bc
            else:
                break
        return c

    def s_mids(mi_month: int) -> List[float]:
        left = None
        right = None
        for s in sol.segments:
            lo, hi = _mi(*s.begin), _mi(*s.end)
            if lo <= mi_month <= hi:
                return [(s.s_lo + s.s_hi) / 2.0]
            if hi < mi_month and (left is None or hi > _mi(*left.end)):
                left = s
            if lo > mi_month and (right is None or lo < _mi(*right.begin)):
                right = s
        return [(s.s_lo + s.s_hi) / 2.0 for s in (left, right) if s is not None]

    groups: Dict[Tuple[str, int], Dict[int, set]] = {}
    for (y, m), why in sol.deviants:
        if why not in ("unexplained", "flutter"):
            continue
        mi_month = _mi(y, m)
        i = idx_of.get(mi_month)
        code = active_code(mi_month)
        if i is None or code is None:
            continue
        offs = basis.code_offsets_by_ym.get(code, {})
        off = offs.get((y, m))
        if off is None:
            continue
        t = series.t_raw[i] + off
        q = series.q[i]
        mids = s_mids(mi_month)
        # Divergence membership requires the PLAIN prediction to fail: a
        # within-eps flutter month that already verifies unpatched is
        # genuine numerical flutter, not compiler divergence, and must not
        # inflate candidate clusters (it did: every flutter-heavy month of
        # USC00402589 became a "candidate" until this guard).
        if any(fp32.pha_qcf(t, s_mid) == q for s_mid in mids):
            continue
        deltas = set()
        for s_mid in mids:
            for delta in (1, -1):
                if fp32.pha_qcf(t + delta, s_mid) == q:
                    deltas.add(delta)
        rec = groups.setdefault((code, m), {})
        rec.setdefault("deltas", []).append(deltas)
        rec.setdefault("members", []).append((y, m))

    cands: List[Tuple[int, str, int, int, Tuple]] = []
    for (code, m), rec in groups.items():
        delta_sets = rec.get("deltas", [])
        members = rec.get("members", [])
        if len(delta_sets) < KE_MIN_REPEATS:
            continue
        # Patch ONLY the member year-months that a given delta reconciles:
        # the knife edge is value-dependent (nint flips for some years'
        # data values, not all), so a whole-month vector patch would
        # corrupt the years that rounded our way (52 of 60 in the
        # synthetic ifort regression).  Members whose delta set is empty
        # are within-eps flutter that already verifies -- they are simply
        # not patched, and must not veto the cluster (they did when a
        # unanimous intersection was required, which silenced detection on
        # the five real divergence stations whose plain readings sweep all
        # near-boundary years into the flutter class).
        for delta in (1, -1):
            sub = [ym for ym, ds in zip(members, delta_sets) if delta in ds]
            if len(sub) >= KE_MIN_REPEATS:
                cands.append((len(sub), code, m, delta, tuple(sorted(sub))))
    cands.sort(reverse=True)
    return [(c, m, d, yms_t) for _n, c, m, d, yms_t in cands]


def solve_tob_station(
    qcu: Dict[Tuple[int, int], int],
    qcf: Dict[Tuple[int, int], int],
    bases: Sequence,
    blend_fn: Optional[Callable] = None,
    qcf_flags: Optional[Dict[Tuple[int, int], str]] = None,
    sid: str = "",
    hints: Optional[List[Tuple[Tuple[int, int, int], str]]] = None,
    vintage_hints: Optional[List[Tuple[Tuple[int, int, int], str]]] = None,
    edge_mis: Optional[frozenset] = None,
) -> Solution:
    """Full TOB+PHA decomposition; bases is a list of tob_basis.Basis.

    `hints` are PHR-documented (date, code) change events -- used only to
    RANK candidates (two-blend repair code ordering); never constraints, and
    solutions remain bit-exact regardless of hints.

    `vintage_hints` (Phase 2, §9) are cross-vintage (date, code) events from a
    donor's adoptable hint regimes.  They influence the solve (enumeration
    preference §9.3, hint_mis merge, hinted-boundary retry §9.4).  To keep the
    laundering guard (§9.1) exact, the station is solved TWICE -- once WITHOUT
    vintage hints (the reference) and once WITH -- and the results compared by
    rank: a strict improvement keeps its natural class; an equal-rank but
    structurally different reading is a POLICY adoption whose differing regimes
    are marked ``hint_influenced`` (exported as ``residual-proven-hinted``).
    The solve is NEVER worse than the vintage-hint-free reference.

    Public entry: a single-exit wrapper.  ``_solve_tob_station_impl`` runs the
    search (it returns early on a clean-exact reading, so evidence capture
    cannot live at the tail of the search body); this wrapper attaches the
    evidence block from the chosen coordinate's basis on the way out.
    """
    series = prepare_series(sid, qcu, qcf, qcf_flags)
    sol = _solve_tob_station_impl(series, bases, blend_fn, sid, hints, None, edge_mis)
    if vintage_hints:
        hinted = _solve_tob_station_impl(
            series, bases, blend_fn, sid, hints, vintage_hints, edge_mis
        )
        sol = _select_hint_influenced(sol, hinted)
    if sol.coord_index is not None:
        sol.evidence = _solution_evidence(
            series, _tobo_arrays(series, bases[sol.coord_index]), sol
        )
    return sol


def _regime_signature(sol: Solution):
    return tuple((tuple(r.begin), r.code, r.blend_day) for r in sol.regimes)


def _select_hint_influenced(base: Solution, hinted: Solution) -> Solution:
    """§9.1 laundering guard: choose between the vintage-hint-free reference
    (`base`) and the vintage-hinted reading (`hinted`), marking policy
    adoptions.  Never returns a reading worse than `base`."""
    if hinted is None or hinted.coord_index is None:
        return base
    if base.coord_index is None:
        hinted.hint_influenced = {tuple(r.begin) for r in hinted.regimes}
        hinted.audits.append("vintage-hint-adopted")
        return hinted
    rb, rh = _rank_key(base), _rank_key(hinted)
    if rh < rb:
        # Strictly better: the data genuinely constrains this reading once the
        # hint suggested trying it -- keep the natural class (§9.1).
        hinted.audits.append("vintage-hint-strict-improvement")
        return hinted
    if rh > rb:
        return base  # never regress below the reference
    if _regime_signature(base) == _regime_signature(hinted):
        return base  # equal rank, identical structure -> no influence
    # Equal rank, different structure: a vintage hint tipped a tie (policy).
    base_sig = {(tuple(r.begin), r.code, r.blend_day) for r in base.regimes}
    hinted.hint_influenced = {
        tuple(r.begin)
        for r in hinted.regimes
        if (tuple(r.begin), r.code, r.blend_day) not in base_sig
    }
    hinted.audits.append("vintage-hint-policy-adoption")
    return hinted


def _solve_tob_station_impl(
    series: Series,
    bases: Sequence,
    blend_fn: Optional[Callable],
    sid: str,
    hints: Optional[List[Tuple[Tuple[int, int, int], str]]],
    vintage_hints: Optional[List[Tuple[Tuple[int, int, int], str]]] = None,
    edge_mis: Optional[frozenset] = None,
) -> Solution:
    upfront = [(_ym(mi), "qcf-only-month") for mi in series.qcf_only]
    # §9.2: vintage hints merge into hint_mis and the tie-break machinery.
    combined_hints = list(hints or []) + list(vintage_hints or [])
    hint_mis = frozenset(_mi(d[0], d[1]) for d, _c in combined_hints)
    # §9.3: enumeration preference.  A vintage hint (date D, code H) says H is
    # active for months >= D.  In the BACKWARD search a switch to c2 at index i
    # closes a regime covering months <= i, so H should be preferred as the
    # switch target at every index i AT OR AFTER index(D) up to the next hint --
    # a carry-forward preference.  It only tips EQUAL-cost states (the
    # double-solve guard makes it safe); absent (no vintage hints) => None =>
    # byte-identical enumeration order.
    hint_code_at: Optional[Dict[int, str]] = None
    if vintage_hints:
        pos = {mi: i for i, mi in enumerate(series.months)}
        idx_code = sorted(
            (pos[_mi(d[0], d[1])], c)
            for d, c in vintage_hints
            if _mi(d[0], d[1]) in pos
        )
        if idx_code:
            hint_code_at = {}
            cur = None
            j = 0
            for i in range(len(series.months)):
                while j < len(idx_code) and idx_code[j][0] <= i:
                    cur = idx_code[j][1]
                    j += 1
                if cur is not None:
                    hint_code_at[i] = cur

    # Phase 1: PLAIN attempts across ALL coordinates; phase 2: knife-edge
    # patched attempts.  Any plain-exact solution beats any patched-exact
    # one -- a patch is an admission our basis disagrees with NOAA's build,
    # so it must never shadow a coordinate that explains the data unpatched.
    import copy as _copy

    best_sol: Optional[Solution] = None
    best_by_coord: Dict[int, Solution] = {}
    # (code, month, delta, n_months) cells where a PATCHED basis would have
    # been fully exact -- documented on the plain winner, never emitted.
    divergence_cands: List[Tuple[str, int, int, int]] = []

    pop_spent = 0  # cumulative search effort across all attempts

    def _run(
        use_basis,
        coord_index,
        ke_patch,
        allow_deep: bool = True,
        allow_noblend: bool = True,
        diagnostic: bool = False,
    ) -> Optional[Solution]:
        """One basis's attempts (both seeds, plus recovery passes).

        diagnostic=True (knife-edge-patched bases): the attempt may not
        update best_sol/best_by_coord and its result is never returned as a
        final solution -- the caller only learns whether a fully-exact
        patched reading EXISTS, to document it on the plain winner as a
        compiler-divergence candidate.  The emitted pipeline must always
        self-verify (TOBMain + .his reproduce the solution's implied
        intermediate), which a patched reading by construction cannot.

        A cumulative pop budget bounds total per-station search effort
        (including diagnostic sweeps) once a best solution exists.
        """
        nonlocal best_sol, pop_spent
        for seed_zero in (True, False):
            if pop_spent > POP_BUDGET and best_sol is not None:
                return None
            res = _solve_with_basis(
                series, use_basis, seed_zero, hint_mis=hint_mis,
                hint_code_at=hint_code_at,
            )
            deep = False
            if res is None and allow_deep:
                # deep-search retry: rare many-changepoint stations need a
                # bigger frontier; one order of magnitude, once.
                res = _solve_with_basis(
                    series,
                    use_basis,
                    seed_zero,
                    cap=SEARCH_POP_CAP * 10,
                    hint_mis=hint_mis,
                    hint_code_at=hint_code_at,
                )
                deep = res is not None
            if res is None:
                pop_spent += SEARCH_POP_CAP  # capped-out search still cost us
                continue
            pop_spent += res["pops"]
            sol = _solution_from_search(
                series,
                use_basis,
                coord_index,
                res["state"],
                upfront,
                seed_zero,
                blend_fn,
                sid,
            )
            if sol is None:
                continue
            sol.stats["pops"] = res["pops"]
            if deep:
                sol.audits.append("deep-search")
            if allow_noblend and any(
                why == "blend-unresolved" for _ym2, why in sol.deviants
            ):
                # The winning trace used a blend the resolver could not date:
                # rerun without the blend branch -- a trial-regime reading
                # the blend was shadowing may now win outright.
                res2 = _solve_with_basis(
                    series, use_basis, seed_zero, no_blend=True, hint_mis=hint_mis,
                    hint_code_at=hint_code_at,
                )
                if res2 is not None:
                    pop_spent += res2["pops"]
                    sol2 = _solution_from_search(
                        series,
                        use_basis,
                        coord_index,
                        res2["state"],
                        upfront,
                        seed_zero,
                        blend_fn,
                        sid,
                    )
                    if sol2 is not None and _rank_key(sol2) < _rank_key(sol):
                        sol2.stats["pops"] = res2["pops"]
                        sol2.audits.append("no-blend-rerun")
                        sol = sol2
            if ke_patch is not None:
                sol.knife_edges.append(ke_patch)
                sol.audits.append(
                    f"knife-edge:{ke_patch[0]}:m{ke_patch[1]}:{ke_patch[2]:+d}"
                )
            if diagnostic:
                # Diagnostic hit: the patched reading is fully exact (no
                # unexplained months) and would have outranked the plain
                # winner -- exactly the readings that used to be adopted
                # (and then failed the pipeline self-verification gate).
                if (
                    sol.exact
                    and not any(w == "unexplained" for _ym2, w in sol.deviants)
                    and (best_sol is None or _rank_key(sol) < _rank_key(best_sol))
                ):
                    return sol
                continue
            if best_sol is None or _rank_key(sol) < _rank_key(best_sol):
                best_sol = sol
            prev = best_by_coord.get(coord_index)
            if prev is None or _rank_key(sol) < _rank_key(prev):
                best_by_coord[coord_index] = sol
            if (
                _clean(sol)
                and not _systematic_flutter(sol)
                and not _hinted_deviant(sol, hint_mis)
            ):
                return sol
        return None

    usable = [
        (ci, b) for ci, b in enumerate(bases) if getattr(b, "station_adjusted", True)
    ]
    for coord_index, basis in usable:
        primary = coord_index == usable[0][0]
        exact_sol = _run(
            basis, coord_index, None, allow_deep=primary, allow_noblend=primary
        )
        if exact_sol is not None:
            return exact_sol
    if best_sol is None or not _clean(best_sol) or _systematic_flutter(best_sol):
        for coord_index, basis in usable:
            cands: List[Tuple] = []
            ke = _knife_edge_prescan(series, basis)
            if ke is not None:
                cands.append((ke[1], ke[2], ke[3]))  # whole-month (constant delta)
            # Mid-record generalization: derive candidates from the best
            # plain reading's same-calendar-month deviant clusters (the
            # prescan is tail-window-only -- see _ke_candidates_from_solution
            # for the root cause it covers).
            ref = best_by_coord.get(coord_index) or best_sol
            if ref is not None:
                for c_code, c_mo, c_delta, c_yms in _ke_candidates_from_solution(
                    series, ref, basis
                ):
                    cand = (c_code, c_mo, c_delta, c_yms)
                    if cand not in cands:
                        cands.append(cand)
            for cand in cands:
                if len(cand) == 3:
                    ke_code, ke_mo, ke_delta = cand
                    only_yms = None  # prescan: constant delta every year
                else:
                    ke_code, ke_mo, ke_delta, only_yms = cand
                patched = _copy.deepcopy(basis)
                n_patched = 0
                for ym in list(patched.code_offsets_by_ym.get(ke_code, {})):
                    if ym[1] == ke_mo and (only_yms is None or ym in only_yms):
                        patched.code_offsets_by_ym[ke_code][ym] += ke_delta
                        n_patched += 1
                primary = coord_index == usable[0][0]
                # DIAGNOSTIC ONLY (compiler-divergence detection): a patched
                # basis asserts values our binary cannot produce, so a
                # patched reading must never be emitted -- the pipeline
                # would not self-verify.  Detected cells feed a gate-row
                # derivation attempt: trig-expressible cells become llvm
                # gate rows (e.g. row 5, USC00222099's April cell); cells
                # that are not trig-expressible (ifort codegen effects --
                # ulp-sweep and SVML-probe evidence in work/gate_scratch2/)
                # remain DOCUMENTED divergences on the plain winner.
                diag_sol = _run(
                    patched,
                    coord_index,
                    None,
                    allow_deep=primary,
                    allow_noblend=primary,
                    diagnostic=True,
                )
                if diag_sol is not None:
                    key = (ke_code, ke_mo, ke_delta)
                    if key not in [d[:3] for d in divergence_cands]:
                        divergence_cands.append((ke_code, ke_mo, ke_delta, n_patched))
                    # Minimal documentation: candidates are ordered largest
                    # cluster first, and the first sufficient patch ends the
                    # search (the original adoption semantics).  Documenting
                    # every independently-sufficient patch would sweep
                    # ordinary flutter months (which any +-1 nudge can
                    # "explain") into the divergence record.
                    break
            if divergence_cands:
                break

    if best_sol is None:
        # Step-2' safety net: greedy non-backtracking construction, so cap
        # exhaustion degrades to a structured (costed, auditable) solution
        # instead of the sentinel.
        for coord_index, basis in usable:
            gc = _greedy_construct(series, basis)
            if gc is None:
                continue
            code_at, dev = gc
            sol = _package_solution(
                series,
                basis,
                coord_index,
                code_at,
                dev,
                [],
                True,
                upfront,
                blend_fn,
                sid,
                extra_audits=["fallback-construction"],
            )
            if best_sol is None or _rank_key(sol) < _rank_key(best_sol):
                best_sol = sol
    if best_sol is None:
        return Solution(
            sid,
            "tob",
            None,
            [],
            [],
            upfront,
            [],
            ["no-solution"],
            (10**6, len(upfront), 0, 0, 0, 0),
            False,
            stats={"n_months": len(series.months)},
        )
    # Defensible-coordinate rule: alternate coordinates are only allowed to
    # WIN by reaching CLEAN exactness.  When nothing is cleanly exact, keep
    # the inventory (coord 0) reading even if an alternate coordinate scored
    # a lower cost.
    if 0 in best_by_coord and not _clean(best_sol):
        best_sol = best_by_coord[0]
    # Hint-upgrade pass: when the winning reading carries flutter (or
    # unexplained) months adjacent to a PHR-documented change date, the
    # documented reading may be fully bit-exact (zero deviants of any kind)
    # at the cost of extra tob boundaries -- e.g. a documented tiny
    # excursion whose only offset difference is the knife-edge month.  Rerun
    # once with flutter priced as deviant so that reading can be found;
    # accept ONLY a completely deviant-free result (never trade exactness).
    if (
        best_sol is not None
        and best_sol.coord_index is not None
        and _hinted_deviant(best_sol, hint_mis)
    ):
        ub = bases[best_sol.coord_index]
        for seed_zero in (True, False):
            res = _solve_with_basis(
                series,
                ub,
                seed_zero,
                hint_mis=hint_mis,
                flutter_as_deviant=True,
            )
            if res is None:
                continue
            sol = _solution_from_search(
                series,
                ub,
                best_sol.coord_index,
                res["state"],
                upfront,
                seed_zero,
                blend_fn,
                sid,
            )
            if sol is not None and sol.exact and not sol.deviants:
                sol.audits.append("hint-upgrade")
                best_sol = sol
                break

    # knife-edge retry: >=3 deviants sharing a calendar month
    best_sol = _knife_edge_retry(series, bases, best_sol, upfront, blend_fn, sid)
    # two-blend short-regime repair on remaining 2-4-month deviant clusters
    if best_sol.coord_index is not None and blend_fn is not None:
        best_sol = _two_blend_repair(
            series,
            bases[best_sol.coord_index],
            best_sol,
            blend_fn,
            upfront,
            sid,
            hints=combined_hints,
        )
    # Phase 2 (§9.4): hinted-boundary retry -- move a boundary onto a vintage
    # hint's code/date when it strictly reduces deviants.  Strict-improvement
    # only; the incumbent is repackaged through _package_solution for a fair
    # like-for-like rank comparison.
    if best_sol.coord_index is not None and blend_fn is not None and vintage_hints:
        best_sol = _hinted_boundary_retry(
            series,
            bases[best_sol.coord_index],
            best_sol,
            vintage_hints,
            blend_fn,
            upfront,
            sid,
        )
    # magnitude tie-break refinement of underdetermined boundaries (equal
    # cost, smaller unexplained cents; hinted dates only)
    if best_sol.coord_index is not None and blend_fn is not None and combined_hints:
        best_sol = _boundary_magnitude_refine(
            series, bases[best_sol.coord_index], best_sol, combined_hints, blend_fn
        )
    # Prefer an edge deviant over a short obs-time flip at a record edge /
    # hiatus / QCF-constraint-gap restart (physical parsimony over exactness).
    if best_sol.coord_index is not None and blend_fn is not None and edge_mis:
        best_sol = _deflip_short_edge_regimes(
            series, bases[best_sol.coord_index], best_sol, edge_mis, upfront,
            blend_fn, sid,
        )
    # Document compiler-divergence candidates on the PLAIN winner: the
    # patched reading is never emitted, but the evidence (a +-1 cell patch
    # that would explain everything, QCF-adjudicated) is preserved for
    # gate-row derivation or as a known ifort-codegen divergence.
    for dv_code, dv_mo, dv_delta, dv_n in divergence_cands:
        if (dv_code, dv_mo, dv_delta) not in [tuple(k) for k in best_sol.knife_edges]:
            best_sol.knife_edges.append((dv_code, dv_mo, dv_delta))
            best_sol.audits.append(
                f"compiler-divergence-candidate:{dv_code}:m{dv_mo}:{dv_delta:+d}:{dv_n}"
            )
    return best_sol


def _boundary_magnitude_refine(
    series: Series,
    basis,
    sol: Solution,
    hints: List[Tuple[Tuple[int, int, int], str]],
    blend_fn,
) -> Solution:
    """Tie-break-only boundary refinement (USC00012727's shape): a deviant
    month immediately before a regime boundary, separated from it only by
    months with no data, leaves the boundary placement underdetermined by
    the constraints.  When a PHR hint documents the boundary's code change
    at a date inside the deviant month, re-anchor the boundary there iff the
    deviant's unexplained magnitude under the day-blend prediction is
    STRICTLY smaller.  Cost tuple is unchanged by construction (same
    deviants, same regime/changepoint counts) -- this is purely the
    equal-cost magnitude tie-break plus documented-date preference."""
    if not sol.deviants or not sol.regimes:
        return sol
    idx_of = {m: i for i, m in enumerate(series.months)}
    regs = sorted(sol.regimes, key=lambda r: r.begin)
    changed = False
    refined_mag: Dict[Tuple[int, int], int] = {}
    for k in range(1, len(regs)):
        r = regs[k]
        prev = regs[k - 1]
        bmi = _mi(r.begin[0], r.begin[1])
        for di, ((dy, dm), why) in enumerate(sol.deviants):
            if why not in ("unexplained", "blend-unresolved", "no-single-S"):
                continue
            dmi = _mi(dy, dm)
            if not (0 < bmi - dmi <= 3):
                continue
            # months strictly between deviant and boundary must carry no data
            if any(mm in idx_of for mm in range(dmi + 1, bmi)):
                continue
            i = idx_of.get(dmi)
            if i is None:
                continue
            hinted_days = sorted(
                {
                    d[2]
                    for d, c in hints
                    if (d[0], d[1]) == (dy, dm) and c == r.code and d[2] > 1
                }
            )
            if not hinted_days:
                continue
            left = None
            right = None
            covering = None
            for s in sol.segments:
                lo = _mi(*s.begin)
                hi = _mi(*s.end)
                if lo <= dmi <= hi:
                    covering = s
                elif hi < dmi and (left is None or hi > _mi(*left.end)):
                    left = s
                elif lo > dmi and (right is None or lo < _mi(*right.begin)):
                    right = s
            if covering is not None:
                s_mids = [(covering.s_lo + covering.s_hi) / 2.0]
            else:
                s_mids = [
                    (s.s_lo + s.s_hi) / 2.0 for s in (left, right) if s is not None
                ]
            if not s_mids:
                continue
            # current reading's prediction for the deviant month: the
            # PREVIOUS era's code (the boundary currently sits after it)
            cur_best = None
            off = basis.code_offsets_by_ym.get(prev.code, {}).get((dy, dm))
            if off is not None:
                for s_mid in s_mids:
                    pred = fp32.pha_qcf(series.t_raw[i] + off, fp32.f32(s_mid))
                    dmag0 = abs(series.q[i] - pred)
                    if cur_best is None or dmag0 < cur_best:
                        cur_best = dmag0
            try:
                per_day = blend_fn(basis.coord, (dy, dm), prev.code, r.code)
            except Exception:
                continue
            for day in hinted_days:
                off = (per_day.get(day) or {}).get((dy, dm))
                if off is None:
                    continue
                dmag = None
                for s_mid in s_mids:
                    pred = fp32.pha_qcf(series.t_raw[i] + off, fp32.f32(s_mid))
                    dd2 = abs(series.q[i] - pred)
                    if dmag is None or dd2 < dmag:
                        dmag = dd2
                if cur_best is None or dmag < cur_best:
                    r.begin = (dy, dm, day)
                    r.blend_day = day
                    sol.audits.append(f"magnitude-refined@{dy:04d}-{dm:02d}-{day:02d}")
                    changed = True
                    cur_best = dmag
                    refined_mag[(dy, dm)] = dmag
    if changed:
        sol.regimes = regs
        sol.stats["deviant_magnitude"] = _deviant_magnitude(
            series, basis, sol, overrides=refined_mag
        )
    return sol


def _greedy_construct(
    series: Series, basis
) -> Optional[Tuple[List[Optional[str]], set]]:
    """Step-2' safety net: non-backtracking timeline construction.

    Partition the record (walking backward) into maximal single-S runs,
    each assigned the code with the longest run from that point (sticky:
    the incumbent code is kept when its run is as long).  Months no code
    explains become deviants.  Always terminates in O(n * codes); the
    result is packaged and costed like any search solution -- structured
    output instead of the no-solution sentinel.
    """
    tobos = _tobo_arrays(series, basis)
    codes = sorted(tobos, key=_code_rank)
    n = len(series.months)
    if n == 0 or not codes:
        return None

    def run_len(code: str, j: int) -> int:
        iv: Optional[Interval] = FULL
        k = j
        cnt = 0
        while k >= 0:
            off = tobos[code][k]
            if off is None:
                break
            ni = _isect(iv, _psi(series.t_raw[k] + off, series.q[k]))
            if ni is None:
                break
            iv = ni
            cnt += 1
            k -= 1
        return cnt

    code_at: List[Optional[str]] = [None] * n
    dev: set = set()
    i = n - 1
    prev_code: Optional[str] = None
    while i >= 0:
        best_code, best_len = None, 0
        if prev_code is not None:
            best_code, best_len = prev_code, run_len(prev_code, i)
        for c in codes:
            if c == prev_code:
                continue
            l = run_len(c, i)
            if l > best_len:
                best_code, best_len = c, l
        if best_len == 0:
            dev.add(i)
            i -= 1
            continue
        for k in range(i, i - best_len, -1):
            code_at[k] = best_code
        prev_code = best_code
        i -= best_len
    if all(c is None for c in code_at):
        return None
    # fill deviant gaps with the nearest later (newer) assigned code so the
    # regime timeline stays well-formed
    nxt: Optional[str] = None
    for k in range(n - 1, -1, -1):
        if code_at[k] is not None:
            nxt = code_at[k]
        elif nxt is not None:
            code_at[k] = nxt
    for k in range(n):
        if code_at[k] is None:
            code_at[k] = nxt if nxt is not None else codes[0]
    return code_at, dev


def _solution_from_search(
    series: Series,
    basis,
    coord_index: int,
    state: _State,
    upfront,
    seed_zero: bool,
    blend_fn,
    sid: str,
) -> Optional[Solution]:
    start_code, switches, blends, dev_idx, flut_idx, cp_idx = _decode_trace(
        series, basis, state
    )
    # Build code timeline over constraint indices (time-ascending).
    n = len(series.months)
    code_at: List[Optional[str]] = [None] * n
    # walk newest -> oldest replaying events in order of constraint index
    events = []  # (idx, code_older)
    for i, c2, _c0, _wcp in switches:
        events.append((i, c2))
    for i, c2, _cnew in blends:
        events.append((i, c2))
    events.sort(reverse=True)
    cur_code = start_code
    ev = 0
    for i in range(n - 1, -1, -1):
        while ev < len(events) and events[ev][0] == i:
            cur_code = events[ev][1]
            ev += 1
        code_at[i] = cur_code

    return _package_solution(
        series,
        basis,
        coord_index,
        code_at,
        set(dev_idx),
        blends,
        seed_zero,
        upfront,
        blend_fn,
        sid,
        flutter_set=set(flut_idx),
    )


def _flutterize_singletons(
    series: Series,
    segs: List[_Seg],
    tobo_of: Callable[[int], Optional[int]],
) -> Tuple[List[_Seg], List[int]]:
    """Reclassify sub-3-constraint boundary segments as flutter when every
    member month misses a NEIGHBOR segment's interval by < FLUTTER_EPS
    (USC00034572's 1975-03: 3e-6 miss against the following era; the
    singleton otherwise costs a hard-short, which ranks above everything and
    warps the reading).  Neighbors are merged afterwards when their
    intervals intersect (no real S step remains)."""
    changed = True
    flut: List[int] = []
    while changed:
        changed = False
        for k, seg in enumerate(segs):
            if len(seg.idxs) >= HARD_SHORT_CONSTRAINTS:
                continue
            for nb in (
                segs[k - 1] if k > 0 else None,
                segs[k + 1] if k + 1 < len(segs) else None,
            ):
                if nb is None:
                    continue
                ok = True
                for i in seg.idxs:
                    off = tobo_of(i)
                    if off is None:
                        ok = False
                        break
                    gap = _flutter_gap(_psi(series.t_raw[i] + off, series.q[i]), nb.iv)
                    if gap is None or gap >= FLUTTER_EPS:
                        ok = False
                        break
                if ok:
                    flut.extend(seg.idxs)
                    segs = segs[:k] + segs[k + 1 :]
                    # merge the now-adjacent neighbours if no S step remains
                    if 0 < k < len(segs):
                        m_iv = _isect(segs[k - 1].iv, segs[k].iv)
                        if m_iv is not None:
                            merged = _Seg(sorted(segs[k - 1].idxs + segs[k].idxs), m_iv)
                            segs = segs[: k - 1] + [merged] + segs[k + 1 :]
                    changed = True
                    break
            if changed:
                break
    return segs, sorted(flut)


def _canonicalize_regimes(
    series: Series,
    code_at: List[Optional[str]],
    tobos: Dict[str, List[Optional[int]]],
    excluded: set,
) -> List[Optional[str]]:
    """Vacuous-flip guard: merge any regime whose offsets are identical to
    its predecessor's over EVERY observed month of its span back into the
    predecessor (e.g. an invented 00RS regime observed only Nov-Mar, where
    tobchg makes 00RS ≡ 00SS; or 24HR vs 00HR).  Such flips carry zero
    evidence and exist only as cost noise."""
    n = len(code_at)
    changed = True
    while changed:
        changed = False
        # find runs
        runs: List[Tuple[int, int, str]] = []  # (start, end, code)
        s = 0
        for i in range(1, n + 1):
            if i == n or code_at[i] != code_at[s]:
                if code_at[s] is not None:
                    runs.append((s, i - 1, code_at[s]))
                s = i
        for r in range(1, len(runs)):
            s0, e0, c_prev = runs[r - 1]
            s1, e1, c_cur = runs[r]
            if c_prev == c_cur:
                continue
            obs = [i for i in range(s1, e1 + 1) if i not in excluded]
            if not obs:
                continue
            prev_arr = tobos.get(c_prev)
            cur_arr = tobos.get(c_cur)
            if prev_arr is None or cur_arr is None:
                continue
            if all(prev_arr[i] is not None and prev_arr[i] == cur_arr[i] for i in obs):
                for i in range(s1, e1 + 1):
                    code_at[i] = c_prev
                changed = True
                break
    return code_at


def _package_solution(
    series: Series,
    basis,
    coord_index: int,
    code_at: List[Optional[str]],
    dev_set: set,
    blends: List[Tuple[int, str, str]],
    seed_zero: bool,
    upfront,
    blend_fn,
    sid: str,
    extra_audits: Optional[List[str]] = None,
    flutter_set: Optional[set] = None,
) -> Solution:
    """Package a code timeline + deviant set into a full Solution
    (segments, blend resolution, regimes, cost, audits).  Shared by the
    branch&bound decode path and the greedy fallback constructor."""
    n = len(series.months)
    blend_set = {i for i, _, _ in blends}
    flutter_set = set(flutter_set or ())
    tobos = _tobo_arrays(series, basis)

    code_at = _canonicalize_regimes(
        series, list(code_at), tobos, dev_set | flutter_set | blend_set
    )

    def tobo_of(i: int) -> Optional[int]:
        arr = tobos.get(code_at[i])
        return arr[i] if arr is not None else None

    segs, extra_devs, extra_flut = _segments_from_pairs(
        series,
        tobo_of,
        dev_set | flutter_set,
        blend_set,
        seed_zero=seed_zero,
    )
    segs, singleton_flut = _flutterize_singletons(series, segs, tobo_of)
    dev_all = sorted((dev_set | set(extra_devs)) - flutter_set - set(singleton_flut))
    flut_all = sorted(flutter_set | set(extra_flut) | set(singleton_flut))

    # Blend resolution.  Collect the full feasible-day set per resolved blend
    # constraint index so the winner's evidence can record it (the returned
    # first day is unchanged by collection -- determinism preserved).
    regime_blend_days: Dict[int, int] = {}
    blend_deviants: List[int] = []
    blend_days_feasible: Dict[int, List[int]] = {}
    for i, c_old, c_new in blends:
        day = None
        if blend_fn is not None:
            feas: List[int] = []
            day = _resolve_blend(
                series, basis, i, c_old, c_new, segs, blend_fn, collect_days=feas
            )
            if day is not None:
                blend_days_feasible[i] = feas
        if day is None:
            blend_deviants.append(i)
        else:
            regime_blend_days[i] = day

    deviants = (
        upfront
        + [(_ym(series.months[i]), "unexplained") for i in dev_all]
        + [(_ym(series.months[i]), "blend-unresolved") for i in blend_deviants]
        + [(_ym(series.months[i]), "flutter") for i in flut_all]
    )

    # Regimes (time-ascending).  A code boundary detected at constraint index
    # j means the newer code's first fully-explained month is months[j]; when
    # the preceding constraint index j-1 was a blend month with a resolved
    # switch day d, the regime actually begins (ym(months[j-1]), d) -- inside
    # the blend month -- so the emitted .his reproduces TOBMain's day-weighted
    # blend there.
    regimes: List[RegimeOut] = []
    boundaries: List[Tuple[Tuple[int, int, int], str, Optional[int], int]] = []
    prev_code = None
    for j in range(n):
        c = code_at[j]
        if c != prev_code:
            bday: Optional[int] = None
            if j > 0 and (j - 1) in regime_blend_days:
                bday = regime_blend_days[j - 1]
                y, m = _ym(series.months[j - 1])
            else:
                y, m = _ym(series.months[j])
            boundaries.append(((y, m, bday if bday else 1), c, bday, j))
            prev_code = c
    for begin, c, bday, _j in boundaries:
        regimes.append(RegimeOut(begin=begin, end=None, code=c, blend_day=bday))

    tob_changes = max(0, len(boundaries) - 1)
    short_regs = 0
    for k in range(len(boundaries)):
        lo = series.months[boundaries[k][3]]
        hi = (
            series.months[boundaries[k + 1][3]] - 1
            if k + 1 < len(boundaries)
            else series.months[-1]
        )
        if hi - lo + 1 < MIN_REGIME_MONTHS and len(boundaries) > 1:
            short_regs += 1

    cost = _cost_of(
        series,
        segs,
        dev_all + blend_deviants,
        tob_changes,
        short_regs,
        flutter=len(flut_all),
    )
    audits = list(extra_audits or []) + ([] if seed_zero else ["tail-anchor-fallback"])
    for s in segs:
        lo = series.months[s.idxs[0]]
        hi = series.months[s.idxs[-1]]
        if _count_visible(series, lo, hi) < MIN_SEG_VISIBLE and len(segs) > 1:
            audits.append(f"short-segment@{_ym(lo)}")
    # Hard-short segments audit rather than break exactness; they still
    # rank poorly via cost[0] during hypothesis search.
    for s in segs:
        if len(s.idxs) < HARD_SHORT_CONSTRAINTS:
            audits.append(f"hard-short-segment@{_ym(series.months[s.idxs[0]])}")
    # S-EXCURSION guard (PHA physics): a middle segment B between A and C
    # with A.S and C.S overlapping, B under MIN_SEG_VISIBLE process-visible
    # months, and a code flip X->Y->X spanning B cannot be real PHA structure
    # (changepoints that close with no S progress and no deletions violate
    # min-length); such readings exist only as flutter-avoidance artifacts.
    # NOTE it does NOT fire on genuine short trials whose S continues
    # unbroken (e.g. USC00487105's 15HR trial sits INSIDE one segment -- no
    # A/B/C boundary exists there).
    for k in range(1, len(segs) - 1):
        a, b, c = segs[k - 1], segs[k], segs[k + 1]
        if _isect(a.iv, c.iv) is None:
            continue
        lo = series.months[b.idxs[0]]
        hi = series.months[b.idxs[-1]]
        if _count_visible(series, lo, hi) >= MIN_SEG_VISIBLE:
            continue
        code_a = code_at[a.idxs[-1]]
        code_c = code_at[c.idxs[0]]
        if code_a == code_c and any(code_at[i] != code_a for i in b.idxs):
            cost = (cost[0] + 1,) + cost[1:]
            audits.append(f"s-excursion-artifact@{_ym(lo)}")
    exact = not dev_all and not blend_deviants and not upfront
    sol = Solution(
        sid,
        "tob",
        coord_index,
        regimes,
        _finalize_segments(series, segs),
        deviants,
        [],
        audits,
        cost,
        exact,
        stats={
            "n_months": len(series.months),
            "n_segments": len(segs),
            "n_regimes": len(regimes),
            "seeded": seed_zero,
        },
    )
    sol.stats["deviant_magnitude"] = _deviant_magnitude(series, basis, sol)
    if blend_days_feasible:
        # Keyed by resolved blend constraint index; rides on the winner so
        # _solution_evidence can read the search-blend feasible-day sets.
        sol.stats["blend_days_feasible"] = blend_days_feasible
    return sol


def _deviant_magnitude(
    series: Series,
    basis,
    sol: Solution,
    overrides: Optional[Dict[Tuple[int, int], int]] = None,
) -> int:
    """Total unexplained magnitude: sum over deviant months of the smallest
    |observed - predicted| (cents) over the reading's plausible predictions
    (active regime code, and the neighbouring regime's code at a boundary).

    Used ONLY as a tie-break between readings of equal cost (USC00012727:
    two boundary placements both leave one deviant month; the one leaving
    3 unexplained cents beats the one leaving 11).  Never traded against
    exactness or deviant COUNT."""
    if not sol.deviants or not sol.regimes:
        return 0
    idx_of = {m: i for i, m in enumerate(series.months)}
    bounds = sorted((_mi(r.begin[0], r.begin[1]), r.code) for r in sol.regimes)

    def codes_near(mi_month: int) -> List[str]:
        # READING-CONSISTENT predictions only: the code this reading makes
        # active at the month (the previous code as well when a boundary
        # falls inside the month -- partial-month ambiguity), never a
        # neighbouring era's code (that would let an alternative reading's
        # prediction mask this reading's unexplained magnitude).
        active = None
        prev = None
        for bmi, c in bounds:
            if bmi < mi_month:
                prev = active
                active = c
            elif bmi == mi_month:
                # boundary inside this month: both codes participate
                return [c0 for c0 in (c, active) if c0]
            else:
                break
        return [c0 for c0 in (active,) if c0] or [c for c in (prev,) if c]

    def s_mids_near(mi_month: int) -> List[float]:
        # Covering segment when one exists; otherwise BOTH neighbouring
        # segments -- a deviant month between segments has an equally
        # underdetermined S-boundary placement, so the best prediction is
        # the min over both sides (USC00012727: May 1962 sits between the
        # S=-0.12 and S=0.0 eras; only the new-era side gives the honest
        # 3-cent residual).
        left = None
        right = None
        for s in sol.segments:
            lo = _mi(*s.begin)
            hi = _mi(*s.end)
            if lo <= mi_month <= hi:
                return [(s.s_lo + s.s_hi) / 2.0]
            if hi < mi_month and (left is None or hi > _mi(*left.end)):
                left = s
            if lo > mi_month and (right is None or lo < _mi(*right.begin)):
                right = s
        return [(s.s_lo + s.s_hi) / 2.0 for s in (left, right) if s is not None]

    total = 0
    for (y, m), why in sol.deviants:
        if str(why).startswith("partial-month-evidence") or why == "qcf-only-month":
            continue
        if overrides and (y, m) in overrides:
            # blend-day prediction computed by the boundary refinement --
            # the pure-code prediction is not this reading's model there
            total += overrides[(y, m)]
            continue
        mi_month = _mi(y, m)
        i = idx_of.get(mi_month)
        if i is None:
            continue
        s_mids = s_mids_near(mi_month)
        if not s_mids:
            continue
        best = None
        for code in codes_near(mi_month):
            off = basis.code_offsets_by_ym.get(code, {}).get((y, m))
            if off is None:
                continue
            for s_mid in s_mids:
                pred = fp32.pha_qcf(series.t_raw[i] + off, fp32.f32(s_mid))
                d = abs(series.q[i] - pred)
                if best is None or d < best:
                    best = d
        if best is not None:
            total += best
    return total


def _rank_key(sol: Solution):
    """Full ordering for candidate readings: cost first (unchanged
    semantics), then deviant magnitude as the tie-break."""
    return (tuple(sol.cost), sol.stats.get("deviant_magnitude", 0))


def _resolve_blend(
    series: Series,
    basis,
    i: int,
    c_old: str,
    c_new: str,
    segs,
    blend_fn,
    collect_days: Optional[List[int]] = None,
) -> Optional[int]:
    """Find the switch day whose blended offset satisfies month i's constraint
    within its PHA segment's interval.

    The return value (first feasible day, determinism preserved) is unchanged
    by ``collect_days``.  When ``collect_days`` is given, the scan CONTINUES
    past the first hit only to append every feasible day to it (evidence
    capture); the first appended day equals the returned day.
    """
    mi = series.months[i]
    y, m = _ym(mi)
    # Enclosing segment interval; when the blend month lies OUTSIDE every
    # segment span (it was excluded from constraints), accept a day whose S
    # requirement is consistent with EITHER nearest neighbour segment -- a
    # FULL fallback would accept days needing an S inconsistent with both.
    acceptable: List[Interval] = []
    for s in segs:
        lo = series.months[s.idxs[0]]
        hi = series.months[s.idxs[-1]]
        if lo <= mi <= hi:
            acceptable = [s.iv]
            break
    if not acceptable:
        left_iv = None
        right_iv = None
        for s in segs:
            hi = series.months[s.idxs[-1]]
            lo = series.months[s.idxs[0]]
            if hi < mi:
                left_iv = s.iv  # segs are oldest-first; last such wins
            elif lo > mi and right_iv is None:
                right_iv = s.iv
        acceptable = [iv for iv in (left_iv, right_iv) if iv is not None] or [FULL]
    try:
        per_day = blend_fn(basis.coord, (y, m), c_old, c_new)
    except Exception:
        return None
    first: Optional[int] = None
    for day in sorted(per_day):
        off = per_day[day].get((y, m))
        if off is None:
            continue
        iv = _psi(series.t_raw[i] + off, series.q[i])
        if any(_isect(iv, acc) is not None for acc in acceptable):
            if collect_days is None:
                return day
            collect_days.append(day)
            if first is None:
                first = day
    return first


def _two_blend_repair(
    series: Series,
    basis,
    sol: Solution,
    blend_fn,
    upfront,
    sid: str,
    hints: Optional[List[Tuple[Tuple[int, int, int], str]]] = None,
) -> Solution:
    """Resolve 2-4-month unexplained clusters as a SHORT INTERIOR REGIME with
    mid-month entry AND exit blends (the USC00134063 shape: 24HR ->
    07HR@1990-12-04 -> 18HR@1991-02-09, PHR-confirmed to the day).

    For each contiguous deviant cluster at a regime boundary or interior:
    hypothesize code X spanning it; middle month(s) must fit the FLANKING
    segments' S exactly (no S excursion); entry/exit days resolved via
    blend_fn.  Candidate X order: PHR-hinted codes near the cluster first,
    then basis-offset fit on the middle months, then CODE_PREF.  Priced as 2
    tob_changes + short-regime penalty, which beats the 2-3 deviants it
    replaces under the cost order.
    """
    if blend_fn is None or sol.coord_index is None:
        return sol
    dev_mis = sorted(
        _mi(*ym)
        for ym, why in sol.deviants
        if why in ("unexplained", "blend-unresolved")
    )
    if not dev_mis:
        return sol
    # contiguous clusters of 2-4
    clusters: List[List[int]] = []
    cur = [dev_mis[0]]
    for mi in dev_mis[1:]:
        if mi == cur[-1] + 1:
            cur.append(mi)
        else:
            clusters.append(cur)
            cur = [mi]
    clusters.append(cur)
    clusters = [c for c in clusters if 2 <= len(c) <= 4]
    if not clusters:
        return sol

    tobos = _tobo_arrays(series, basis)
    mi_to_idx = {mi: k for k, mi in enumerate(series.months)}

    def regime_code_at(mi: int) -> Optional[str]:
        code = None
        for r in sol.regimes:
            if _mi(r.begin[0], r.begin[1]) <= mi:
                code = r.code
        return code

    new_regimes = [RegimeOut(r.begin, r.end, r.code, r.blend_day) for r in sol.regimes]
    resolved_clusters = 0
    resolved_months: List[int] = []
    for cluster in clusters:
        idxs = [mi_to_idx[mi] for mi in cluster if mi in mi_to_idx]
        if len(idxs) != len(cluster):
            continue
        left_code = regime_code_at(cluster[0] - 1)
        right_code = regime_code_at(cluster[-1] + 1)
        if left_code is None or right_code is None:
            continue
        # flanking S: either one segment SPANS the cluster (S continues --
        # the common case, since cluster months carry no constraints), or
        # the intersection of the nearest left/right segments applies.
        span_iv = None
        left_iv = right_iv = None
        for s in sol.segments:
            lo = _mi(*s.begin)
            hi = _mi(*s.end)
            if lo < cluster[0] and hi > cluster[-1]:
                span_iv = Interval(s.s_lo, s.s_hi)
            if hi < cluster[0]:
                left_iv = Interval(s.s_lo, s.s_hi)
            elif lo > cluster[-1] and right_iv is None:
                right_iv = Interval(s.s_lo, s.s_hi)
        if span_iv is not None:
            s_acc = span_iv
        else:
            if left_iv is None or right_iv is None:
                continue
            s_acc = _isect(left_iv, right_iv)
        if s_acc is None:
            continue  # S excursion would be needed -- out of scope here

        # candidate codes: PHR hints near the cluster first
        hinted = []
        for hdate, hcode in hints or []:
            hmi = _mi(hdate[0], hdate[1])
            if cluster[0] - 2 <= hmi <= cluster[-1] + 2 and hcode in tobos:
                hinted.append(hcode)
        cands = list(dict.fromkeys(hinted)) + [
            c
            for c in sorted(tobos, key=_code_rank)
            if c not in hinted and c not in (left_code, right_code)
        ]

        middle = idxs[1:-1]
        i_entry, i_exit = idxs[0], idxs[-1]
        for X in cands:
            ivm = s_acc
            ok = True
            for i in middle:
                off = tobos[X][i]
                if off is None:
                    ok = False
                    break
                ivm = _isect(ivm, _psi(series.t_raw[i] + off, series.q[i]))
                if ivm is None:
                    ok = False
                    break
            if not ok:
                continue

            def _day(i: int, c_old: str, c_new: str) -> Optional[int]:
                ym = _ym(series.months[i])
                try:
                    per_day = blend_fn(basis.coord, ym, c_old, c_new)
                except Exception:
                    return None
                for day in sorted(per_day):
                    off = per_day[day].get(ym)
                    if off is None:
                        continue
                    if (
                        _isect(_psi(series.t_raw[i] + off, series.q[i]), ivm)
                        is not None
                    ):
                        return day
                return None

            d1 = _day(i_entry, left_code, X)
            if d1 is None:
                continue
            d2 = _day(i_exit, X, right_code)
            if d2 is None:
                continue
            ey, em = _ym(series.months[i_entry])
            xy, xm = _ym(series.months[i_exit])
            # drop boundaries the pre-repair reading placed inside the cluster
            new_regimes = [
                r
                for r in new_regimes
                if not (cluster[0] <= _mi(r.begin[0], r.begin[1]) <= cluster[-1])
            ]
            new_regimes.append(RegimeOut((ey, em, d1), None, X, d1))
            new_regimes.append(RegimeOut((xy, xm, d2), None, right_code, d2))
            resolved_clusters += 1
            resolved_months.extend(cluster)
            break

    if not resolved_clusters:
        return sol
    new_regimes.sort(key=lambda r: r.begin)
    # dedupe consecutive same-code regimes (right_code may repeat)
    dedup: List[RegimeOut] = []
    for r in new_regimes:
        if dedup and dedup[-1].code == r.code:
            continue
        dedup.append(r)
    resolved_set = set(resolved_months)
    remaining = [
        (ym, why)
        for ym, why in sol.deviants
        if _mi(*ym) not in resolved_set
        or why not in ("unexplained", "blend-unresolved")
    ]
    old = sol.cost
    new_cost = (
        old[0],
        sum(1 for _ym2, w in remaining if w != "flutter"),
        old[2] + 2 * resolved_clusters,
        old[3],
        old[4],
        old[5] + resolved_clusters,
    )
    repaired = Solution(
        sol.station_id,
        sol.kind,
        sol.coord_index,
        dedup,
        sol.segments,
        remaining,
        sol.knife_edges,
        sol.audits + [f"two-blend-repair:x{resolved_clusters}"],
        new_cost,
        exact=not [1 for _ym2, w in remaining if w not in ("flutter",)],
        stats=dict(sol.stats),
    )
    return repaired if repaired.cost < sol.cost else sol


# ---------------------------------------------------------------------------
# Phase 2 (§9.4): hinted-boundary retry.
# ---------------------------------------------------------------------------

_DEMOTING_WHY = ("unexplained", "blend-unresolved", "no-single-S", "repair-merged")


def _dev_month_set(sol: Solution) -> frozenset:
    """Demoting-deviant (y, m) months of a solution (flutter is exempt)."""
    return frozenset(
        (y, m) for (y, m), why in sol.deviants if why in _DEMOTING_WHY
    )


def _reconstruct_package_inputs(series: Series, sol: Solution):
    """Rebuild ``_package_solution`` inputs from a packaged ``Solution``
    (§9.4 reconstruction spec): (code_at, dev_set, flutter_set, blends,
    seed_zero).  A blend regime's begin MONTH is its blend constraint index;
    that month keeps the OLD code in code_at and is recorded as a blend."""
    n = len(series.months)
    pos = {mi: i for i, mi in enumerate(series.months)}
    regs = sorted(sol.regimes, key=lambda r: r.begin)
    starts = []  # (begin_mi, begin_pos, code, blend_day, prev_code)
    prev_code = None
    for r in regs:
        bmi = _mi(r.begin[0], r.begin[1])
        starts.append((bmi, pos.get(bmi), r.code, r.blend_day, prev_code))
        prev_code = r.code

    code_at: List[Optional[str]] = [None] * n
    for i in range(n):
        mi = series.months[i]
        active = None
        for s in starts:
            if s[0] <= mi:
                active = s
            else:
                break
        if active is None:
            continue
        bmi, bpos, code, bd, pc = active
        if bd and mi == bmi and pc is not None:
            code_at[i] = pc  # blend month keeps the old code
        else:
            code_at[i] = code

    blends = []
    for bmi, bpos, code, bd, pc in starts:
        if bd and bpos is not None and pc is not None:
            blends.append((bpos, pc, code))

    dev_set = {
        pos[_mi(y, m)]
        for (y, m), why in sol.deviants
        if why in ("unexplained", "blend-unresolved") and _mi(y, m) in pos
    }
    flutter_set = {
        pos[_mi(y, m)]
        for (y, m), why in sol.deviants
        if why == "flutter" and _mi(y, m) in pos
    }
    seed_zero = bool(sol.stats.get("seeded", False))
    return code_at, dev_set, flutter_set, blends, seed_zero


def _repackage_incumbent(series, basis, sol, upfront, blend_fn, sid):
    """Repackage the incumbent through ``_package_solution`` so candidate and
    incumbent ranks are compared like-for-like (the incumbent's stored cost
    may be a hand-adjusted two-blend-repair tuple).  Returns None on failure."""
    try:
        code_at, dev_set, flut, blends, seed_zero = _reconstruct_package_inputs(
            series, sol
        )
        if any(c is None for c in code_at):
            return None
        return _package_solution(
            series, basis, sol.coord_index, code_at, dev_set, blends,
            seed_zero, upfront, blend_fn, sid, flutter_set=flut,
        )
    except Exception:
        return None


def _hinted_boundary_retry(
    series: Series,
    basis,
    sol: Solution,
    vintage_hints: List[Tuple[Tuple[int, int, int], str]],
    blend_fn,
    upfront,
    sid: str,
) -> Solution:
    """Move a boundary onto a vintage hint's code when it STRICTLY reduces
    deviants (§9.4).  For a hint (date, H) where the incumbent has a boundary
    within +-2 months whose new-era code != H and >=1 demoting/flutter deviant
    sits in the hinted regime's first 3 months, repackage the reading with H
    over that regime's span and accept ONLY on strict ``_rank_key``
    improvement over the REPACKAGED incumbent (never equal-rank)."""
    if sol.coord_index is None or not sol.regimes:
        return sol
    tobos = _tobo_arrays(series, basis)
    pos = {mi: i for i, mi in enumerate(series.months)}
    repacked = _repackage_incumbent(series, basis, sol, upfront, blend_fn, sid)
    if repacked is None or _dev_month_set(repacked) != _dev_month_set(sol):
        return sol  # cannot trust a like-for-like comparison
    inc_rank = _rank_key(repacked)

    base_code_at, base_dev, base_flut, base_blends, seed_zero = (
        _reconstruct_package_inputs(series, sol)
    )
    if any(c is None for c in base_code_at):
        return sol
    dev_flut_mis = {
        _mi(y, m)
        for (y, m), why in sol.deviants
        if why in ("unexplained", "blend-unresolved", "flutter")
    }
    regs = sorted(sol.regimes, key=lambda r: r.begin)

    for hd, H in vintage_hints:
        if H not in tobos or tobos.get(H) is None:
            continue
        hmi = _mi(hd[0], hd[1])
        for k, r in enumerate(regs):
            bmi = _mi(r.begin[0], r.begin[1])
            if abs(bmi - hmi) > 2 or r.code == H:
                continue
            span_lo = bmi
            span_hi = (
                _mi(regs[k + 1].begin[0], regs[k + 1].begin[1]) - 1
                if k + 1 < len(regs)
                else series.months[-1]
            )
            first3_hi = min(span_hi, span_lo + 2)
            first3 = {mi for mi in dev_flut_mis if span_lo <= mi <= first3_hi}
            if not first3:
                continue
            cand_code_at = list(base_code_at)
            for i, mi in enumerate(series.months):
                if span_lo <= mi <= span_hi:
                    cand_code_at[i] = H
            drop = {pos[mi] for mi in first3 if mi in pos}
            cand = _package_solution(
                series,
                basis,
                sol.coord_index,
                cand_code_at,
                base_dev - drop,
                [b for b in base_blends if b[0] not in drop],
                seed_zero,
                upfront,
                blend_fn,
                sid,
                flutter_set=base_flut - drop,
            )
            if cand is not None and _rank_key(cand) < inc_rank:
                cand.audits.append(
                    f"hinted-boundary-retry@{hd[0]:04d}-{hd[1]:02d}-{hd[2]:02d}"
                )
                return cand
    return sol


DEFLIP_MAX_SPAN = 3  # a "short flip" spans at most this many months
# Dissolving a short edge flip may leave AT MOST this many genuinely
# unexplained (non-edge) deviants; more means the flip encodes real structure
# (the months fit the flip code), so it is kept.
DEFLIP_MAX_UNEXEMPT = 1


def _segment_iv_covering(sol: Solution, mi: int) -> Optional[Interval]:
    for s in sol.segments:
        if _mi(*s.begin) <= mi <= _mi(*s.end):
            return Interval(s.s_lo, s.s_hi)
    return None


def _deflip_short_edge_regimes(
    series: Series,
    basis,
    sol: Solution,
    edge_mis: frozenset,
    upfront,
    blend_fn,
    sid: str,
) -> Solution:
    """Prefer an edge deviant over a short obs-time flip at a record edge /
    hiatus / QCF-constraint-gap restart.

    A physical station does not switch obs time for 2-3 months right where the
    constrained record restarts after a long gap; such a short flip is a
    boundary artifact the search prefers only because a 0-deviant reading
    outranks a fewer-regime reading that carries a deviant (and PHA's 18-month
    minimum forbids absorbing the odd months as a short level).  This pass
    dissolves such a flip into its neighbouring code, forcing the months that
    then miss the neighbour's PHA level (by interval, not tolerance) to become
    deviants -- a simpler, still verify-green reading (the deviants are
    PHA-exempt via the solution's deviant set and TOB-reproduced).  Fires ONLY
    when the flip BEGINS at an edge/hiatus month and the result is structurally
    simpler with no new hard/soft-short segments."""
    if sol.coord_index is None or len(sol.regimes) < 2 or not edge_mis:
        return sol
    tobos = _tobo_arrays(series, basis)
    pos = {mi: i for i, mi in enumerate(series.months)}
    cur = sol
    for _ in range(len(sol.regimes)):  # bounded: at most one dissolve per pass
        regs = sorted(cur.regimes, key=lambda r: r.begin)
        if len(regs) < 2:
            break
        code_at, dev_set, flut, blends, seed_zero = _reconstruct_package_inputs(
            series, cur
        )
        if any(c is None for c in code_at):
            break
        cur_dev = _dev_month_set(cur)
        applied = False
        for k, r in enumerate(regs):
            bmi = _mi(r.begin[0], r.begin[1])
            if bmi not in edge_mis:
                continue  # flip must begin at an edge/hiatus/restart month
            # Prefer extending the NEXT regime's code back (restart case);
            # fall back to the previous regime at the record tail.
            if k + 1 < len(regs):
                nxt = regs[k + 1]
                nbmi = _mi(nxt.begin[0], nxt.begin[1])
                ncode = nxt.code
                # absorb the next regime's blend month when it has one
                span_hi_mi = nbmi if nxt.blend_day else nbmi - 1
                probe_mi = nbmi + 1
            elif k > 0:
                prv = regs[k - 1]
                ncode = prv.code
                span_hi_mi = series.months[-1]
                probe_mi = bmi - 1
            else:
                continue
            if ncode == r.code:
                continue
            if span_hi_mi - bmi > DEFLIP_MAX_SPAN:
                continue
            nseg = _segment_iv_covering(cur, probe_mi)
            span_pos = [i for i, mi in enumerate(series.months) if bmi <= mi <= span_hi_mi]
            if not span_pos:
                continue
            alt = list(code_at)
            force = set()
            arr = tobos.get(ncode)
            for i in span_pos:
                alt[i] = ncode
                off = arr[i] if arr is not None else None
                iv = _psi(series.t_raw[i] + off, series.q[i]) if off is not None else None
                if nseg is None or iv is None or _isect(iv, nseg) is None:
                    force.add(i)
            alt_blends = [b for b in blends if not (bmi <= series.months[b[0]] <= span_hi_mi)]
            cand = _package_solution(
                series, basis, sol.coord_index, alt, dev_set | force, alt_blends,
                seed_zero, upfront, blend_fn, sid, flutter_set=flut,
            )
            if cand is None:
                continue
            new_dev = _dev_month_set(cand) - cur_dev
            n_unexempt = sum(1 for (y, m) in new_dev if _mi(y, m) not in edge_mis)
            if (
                len(cand.regimes) < len(regs)
                and cand.cost[0] <= cur.cost[0]
                and cand.cost[5] <= cur.cost[5]
                and all(bmi <= _mi(y, m) <= span_hi_mi for (y, m) in new_dev)
                and n_unexempt <= DEFLIP_MAX_UNEXEMPT
            ):
                cand.audits.append(
                    f"deflip-edge@{r.begin[0]:04d}-{r.begin[1]:02d}:{r.code}->{ncode}"
                )
                cur = cand
                applied = True
                break
        if not applied:
            break
    return cur


def _knife_edge_retry(
    series: Series, bases, sol: Solution, upfront, blend_fn, sid: str
) -> Solution:
    """If >=3 deviants share one calendar month, retry with basis offsets
    patched +-1 for that (code, month) -- the knife-edge signature.

    DIAGNOSTIC ONLY: the patched reading is never returned (the emitted
    pipeline must self-verify; a patched basis asserts values our binary
    cannot produce).  When the patched retry would have been fully exact,
    the cell is documented on the plain solution as a
    compiler-divergence-candidate audit + knife_edges entry, feeding the
    gate-row derivation workflow (trig-expressible cells become llvm gate
    rows; the rest remain documented ifort-codegen divergences)."""
    from collections import Counter

    if not sol.deviants or sol.coord_index is None:
        return sol
    months = Counter(
        m for (y, m), why in sol.deviants if why in ("unexplained", "flutter")
    )
    if not months:
        return sol
    mo, cnt = months.most_common(1)[0]
    if cnt < 3:
        return sol
    basis = bases[sol.coord_index]
    # code active at the deviant months: the regime whose span covers them
    from collections import Counter as _C
    import copy

    dev_yms = [
        ym
        for ym, why in sol.deviants
        if why in ("unexplained", "flutter") and ym[1] == mo
    ]
    votes: _C = _C()
    for ym in dev_yms:
        covering = None
        for r in sol.regimes:
            if (r.begin[0], r.begin[1]) <= ym:
                covering = r.code
        if covering:
            votes[covering] += 1
    if not votes:
        return sol
    code = votes.most_common(1)[0][0]
    for delta in (1, -1):
        patched = copy.deepcopy(basis)
        n_patched = 0
        for ym in list(patched.code_offsets_by_ym.get(code, {})):
            if ym[1] == mo:
                patched.code_offsets_by_ym[code][ym] += delta
                n_patched += 1
        for seed_zero in (True, False):
            res = _solve_with_basis(series, patched, seed_zero)
            if res is None:
                continue
            cand = _solution_from_search(
                series,
                patched,
                sol.coord_index,
                res["state"],
                upfront,
                seed_zero,
                blend_fn,
                sid,
            )
            if (
                cand is not None
                and cand.exact
                and not any(w == "unexplained" for _ym2, w in cand.deviants)
                and cand.cost < sol.cost
            ):
                if (code, mo, delta) not in [tuple(k) for k in sol.knife_edges]:
                    sol.knife_edges.append((code, mo, delta))
                    sol.audits.append(
                        f"compiler-divergence-candidate:{code}:m{mo}:{delta:+d}:{n_patched}"
                    )
                return sol
    return sol
