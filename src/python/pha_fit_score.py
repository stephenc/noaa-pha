#!/usr/bin/env python3
"""Composite PHA-fit score: how well our PHA output reproduces published QCF.

Both QCF and our PHA output are the TOB series with piecewise-constant PHA
adjustments applied (and some months *removed* by QC).  Using our
bit-exact TOB series as the shared baseline, per station we compare three
things between ``qcf - tob`` (NOAA, the target) and ``adj - tob`` (ours):

  1. TRANSITION DATES  -- the months where the adjustment level steps.  The
     priority is exact date alignment: every NOAA transition matched, none
     missing, none extra.  Scored as a Jaccard over transition months.

  2. LEVEL (magnitude)  -- for transitions that ARE date-aligned, how close
     the adjustment levels are.  A NOAA transition with no exact-date match
     is a full miss (0), so this is averaged over ALL target transitions.

  3. DATA REMOVAL  -- months present in TOB that QC dropped.  We want PHA to
     drop exactly the months QCF dropped: Jaccard over removed months.

Each station's composite is a weighted blend (date-priority, then level,
then removal); the tool prints AGGREGATE statistics only.

Baseline (TOB) months are the superset: QCF and our adjusted series are both
subsets of it, so ``present-in-TOB but absent here`` == a removal.

Usage:
  uv run python src/python/pha_fit_score.py \
      --tob data/intermediate/tob/tavg \
      --qcf data/output/qcf/tavg \
      --adj data/output/adj/tavg
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

sys.path.insert(0, str(Path(__file__).resolve().parent))

import ghcn_io  # noqa: E402

Ym = Tuple[int, int]

# Composite weights (must sum to 1.0).  Date alignment is the priority; level
# is the "second half" of the adjustment score; removal alignment is the
# third dimension the reconstruction must also match.
W_DATE = 0.4
W_LEVEL = 0.3
W_REMOVE = 0.3

# Level-closeness tolerance in hundredths of a degree C: a date-matched
# transition whose |level - level| == 0 scores 1.0 and decays linearly to 0
# at LEVEL_TOL.  0 means exact-only (1.0 iff identical, else 0.0).
LEVEL_TOL = 100

# A level change of this size or less, in hundredths of a degree C, is NOT a
# transition.
#
# Both binaries write integer hundredths.  An adjustment that falls near a
# half-cent boundary therefore flips a level by one unit, and that flip is
# emission rounding, not a decision.  NOAA's own output carries these: of the
# level changes it makes one month apart, 88.9% are exactly 0.01 degC, against
# a median of 0.45 degC for changes 24 months or more apart.  Two populations,
# and only the second is a changepoint.
#
# Applied cumulatively in _transitions, so a drift of successive one-unit steps
# still registers once it exceeds the slack.  Exactness of a matched level is
# reported separately as level_exact_among_matched, and is deliberately NOT
# folded into the score.
TRANSITION_SLACK = 1

# Flag segments shorter than pha.adjust.min-length, in OBSERVATIONS.
#
# This is a HEURISTIC, not an impossibility test.  min_seg_length gates whether
# a changepoint's adjustment is ESTIMATED on a given pass; it does not remove
# the changepoint from the set (ChangepointSize.f95:418).  Before the final
# pass a short segment is logged "-skip" and the changepoint survives, and
# AdjustSeries later applies every entry it finds.  That is sufficient on its
# own: the code does not promise what the name implies.
#
# The target series shows them too, and on NON-CONUS stations that is proof:
# TOB comes from COOP observation-time histories, which exist only for the US
# network, so NOAA's TOB there IS QCU by construction and qcf-qcu is their pure
# PHA output.  MX000008157 carries 1.26 degC over 11 observations and
# ITE00002799 carries 0.45 over 8.
#
# For CONUS the same reasoning does NOT hold: we cannot observe the TOB NOAA
# used, so a violation there may be a baseline difference instead.  That makes
# non-CONUS the control group -- its violation rate is the background, and only
# the excess above it on CONUS stations is a candidate TOB error.
#
# The unit is DATA POINTS, counted over the TOB months PHA read, because that
# is what ChangepointSize.f95:407-418 measures.  Calendar months are a lenient
# proxy and the level series undercounts wherever PHA removed a month.
#
# A high count is worth investigating; it is not proof of anything.  Do NOT
# infer a cause from it.  In particular a large count in `ours` does not show
# that adj and tob are unpaired -- adj is a function of the tob it was run
# against, whatever that tob was.
MIN_SEGMENT = 18


def _list_ids(directory: Path) -> Dict[str, Path]:
    out: Dict[str, Path] = {}
    for p in directory.iterdir():
        if p.is_file():
            out[p.name.split(".")[0]] = p
    return out


def _parse_cutoff(s: Optional[str]) -> Optional[Ym]:
    """Parse an inclusive cutoff 'YYYY-MM' (or 'YYYY', meaning that Dec)."""
    if s is None:
        return None
    parts = s.replace("/", "-").split("-")
    if len(parts) == 1:
        return (int(parts[0]), 12)
    y, m = int(parts[0]), int(parts[1])
    if not (1 <= m <= 12):
        raise ValueError(f"cutoff month out of range: {s!r}")
    return (y, m)


def _values(path: Path, cutoff: Optional[Ym] = None) -> Dict[Ym, int]:
    """{(y, m): value} for present (non-missing) months only, restricted to
    months at or before ``cutoff`` when given (so two vintages can be scored
    on a common window)."""
    vals = ghcn_io.read_station_data(path).values
    if cutoff is not None:
        vals = {ym: v for ym, v in vals.items() if ym <= cutoff}
    return vals


def _adjustment(series: Dict[Ym, int], tob: Dict[Ym, int]) -> Dict[Ym, int]:
    """series - tob on the months present in BOTH (integer hundredths C)."""
    return {ym: series[ym] - tob[ym] for ym in series.keys() & tob.keys()}


def _month_index(ym: Ym) -> int:
    """(year, month) -> a monotonic month number, for spacing arithmetic."""
    return ym[0] * 12 + (ym[1] - 1)


def _transitions(adj: Dict[Ym, int]) -> Dict[Ym, int]:
    """Map {month -> level} at each level CHANGE, walking present months in
    time order.  A transition month is the first month of a new level; its
    value is the new level.  The earliest present month is the baseline, not
    a transition.

    A change of one hundredth is emission rounding, not a changepoint: both
    binaries write integer hundredths, so an adjustment near a half-cent
    boundary flips a level by one unit.  Such a change is therefore NOT a
    transition on its own.

    The test is CUMULATIVE, against the level of the last recorded transition
    rather than against the previous month.  A single step of one hundredth is
    ignored, but two steps in the same direction reach 0.02 and do register, so
    a slow drift is not lost.  A step out and back is ignored, which is what
    rounding around a boundary looks like.

    Spacing is NOT altered here.  See MIN_SEGMENT and `impossible_pairs`: a
    pair closer than min-length is reported, never merged.
    """
    out: Dict[Ym, int] = {}
    base: Optional[int] = None
    for ym in sorted(adj):
        lvl = adj[ym]
        if base is None:
            base = lvl
            continue
        if abs(lvl - base) > TRANSITION_SLACK:
            out[ym] = lvl
            base = lvl
    return out


def impossible_pairs(
    tr: Dict[Ym, int], present: Optional[Dict[Ym, int]] = None
) -> List[Tuple[Ym, Ym, int]]:
    """Consecutive transitions separated by fewer than MIN_SEGMENT observations.

    PHA cannot produce these (see MIN_SEGMENT).  Each is returned as
    (earlier, later, observations_between) so the caller can name the stations
    rather than score them silently.

    `present` must be the TOB series, NOT the level series.  PHA tests
    min_seg_length against element_data_process -- the TOB months it read --
    before it deletes anything (ChangepointSize.f95:407-418).  Counting the
    level series instead undercounts wherever PHA later removed a month, and
    so reports violations PHA never committed.  Without it the calendar month
    difference is used, which is a lenient approximation.
    """
    if MIN_SEGMENT <= 0:
        return []
    months = sorted(tr)
    index = {m: i for i, m in enumerate(sorted(present))} if present else None
    out: List[Tuple[Ym, Ym, int]] = []
    for a, b in zip(months, months[1:]):
        gap = (index[b] - index[a]) if index else (_month_index(b) - _month_index(a))
        if gap < MIN_SEGMENT:
            out.append((a, b, gap))
    return out


def _jaccard(a: set, b: set) -> float:
    """|a & b| / |a | b|; 1.0 when both empty (perfectly, vacuously aligned)."""
    if not a and not b:
        return 1.0
    return len(a & b) / len(a | b)


def _level_credit(delta: int) -> float:
    if LEVEL_TOL <= 0:
        return 1.0 if delta == 0 else 0.0
    return max(0.0, 1.0 - abs(delta) / LEVEL_TOL)


class Acc:
    """Aggregate accumulators."""

    def __init__(self) -> None:
        self.n = 0
        self.sum_date = 0.0
        self.sum_level = 0.0
        self.sum_remove = 0.0
        self.sum_composite = 0.0
        # corpus-wide transition counts
        self.t_target = 0
        self.t_matched = 0
        self.t_missing = 0
        self.t_extra = 0
        self.t_level_exact = 0  # exact-level among date-matched
        # Transitions closer than min-length.  PHA cannot emit these, so a
        # non-zero count means the two series do not belong together.
        # Short segments are legitimate PHA output, so only a DISAGREEMENT
        # matters: one series has one where the other does not.
        self.imp_agreed = 0          # same pair in both -- reproduced correctly
        self.imp_missed = 0          # target has it, we do not
        self.imp_invented = 0        # we have it, target does not
        self.imp_missed_stns: list = []
        self.imp_invented_stns: list = []
        # corpus-wide removal counts
        self.r_target = 0
        self.r_matched = 0
        self.r_missing = 0
        self.r_extra = 0
        # perfect stations (composite == 1.0)
        self.n_perfect = 0


def score_station(
    tob: Dict[Ym, int],
    qcf: Dict[Ym, int],
    adj: Dict[Ym, int],
    acc: Acc,
    sink: Optional[list] = None,
    sid: str = "",
) -> None:
    qcf_adj = _adjustment(qcf, tob)
    pha_adj = _adjustment(adj, tob)

    qcf_tr = _transitions(qcf_adj)  # {month: level}
    pha_tr = _transitions(pha_adj)
    T = set(qcf_tr)  # target transition months
    P = set(pha_tr)
    matched = T & P

    date_score = _jaccard(T, P)

    # Level: over ALL target transitions; date-matched -> closeness, else 0.
    if not T:
        level_score = 1.0 if not P else 0.0
    else:
        credit = 0.0
        for ym in matched:
            delta = qcf_tr[ym] - pha_tr[ym]
            credit += _level_credit(delta)
            if delta == 0:
                acc.t_level_exact += 1
        level_score = credit / len(T)

    # Removal: months in TOB dropped by QC (relative to the TOB superset).
    r_qcf = set(tob) - set(qcf)
    r_pha = set(tob) - set(adj)
    remove_score = _jaccard(r_qcf, r_pha)

    composite = W_DATE * date_score + W_LEVEL * level_score + W_REMOVE * remove_score

    acc.n += 1
    acc.sum_date += date_score
    acc.sum_level += level_score
    acc.sum_remove += remove_score
    acc.sum_composite += composite
    if composite >= 1.0:
        acc.n_perfect += 1

    acc.t_target += len(T)
    acc.t_matched += len(matched)
    acc.t_missing += len(T - P)
    acc.t_extra += len(P - T)

    # Count over TOB months: that is what PHA measured the segment against.
    # A short segment BOTH series place is PHA behaviour we reproduced, not a
    # fault -- only the ones they disagree about say anything about the fit.
    q_pairs = {(a, b): g for a, b, g in impossible_pairs(qcf_tr, tob)}
    p_pairs = {(a, b): g for a, b, g in impossible_pairs(pha_tr, tob)}
    for key in q_pairs.keys() & p_pairs.keys():
        acc.imp_agreed += 1
    for key in q_pairs.keys() - p_pairs.keys():
        acc.imp_missed += 1
        if len(acc.imp_missed_stns) < 40:
            a, b = key
            acc.imp_missed_stns.append(
                (sid, q_pairs[key], abs(qcf_tr[b] - qcf_tr[a]))
            )
    for key in p_pairs.keys() - q_pairs.keys():
        acc.imp_invented += 1
        if len(acc.imp_invented_stns) < 40:
            a, b = key
            acc.imp_invented_stns.append(
                (sid, p_pairs[key], abs(pha_tr[b] - pha_tr[a]))
            )

    acc.r_target += len(r_qcf)
    acc.r_matched += len(r_qcf & r_pha)
    acc.r_missing += len(r_qcf - r_pha)
    acc.r_extra += len(r_pha - r_qcf)

    if sink is not None:
        sink.append(
            (
                sid,
                composite,
                date_score,
                level_score,
                remove_score,
                len(T),
                len(matched),
                len(T - P),
                len(P - T),
                len(r_qcf),
            )
        )


def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--tob", required=True, help="TOB baseline dir (*.tob.tavg)")
    ap.add_argument("--qcf", required=True, help="published QCF dir (target)")
    ap.add_argument("--adj", required=True, help="our PHA output dir (adj)")
    ap.add_argument(
        "--stations",
        default=None,
        help="subset of station ids: comma-separated, or @FILE for one id per "
        "line. The falsification test only needs the stations that currently "
        "score 1.0 (a non-perfect station cannot be 'broken'), and restricting "
        "to them cuts the work ~29x versus scoring all 27,793",
    )
    ap.add_argument(
        "--cutoff",
        default=None,
        metavar="YYYY-MM",
        help="inclusive upper month bound; restricts all three series to "
        "months <= this (e.g. 2024-03 to compare vintages on a common window)",
    )
    ap.add_argument(
        "--cutoff-from-adj",
        action="store_true",
        help="set the cutoff to the latest month present anywhere in --adj "
        "(so an older vintage is scored only over the window it covers, not "
        "penalized for months it predates)",
    )
    ap.add_argument(
        "--per-station",
        default=None,
        metavar="TSV",
        help="also write each station's composite and sub-scores here, so the "
        "distribution behind the aggregate is inspectable (is the mean a broad "
        "cluster, or perfect stations plus a bad tail?)",
    )
    return ap.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> int:
    args = parse_args(argv)
    cutoff = _parse_cutoff(args.cutoff)
    tob_map = _list_ids(Path(args.tob))
    qcf_map = _list_ids(Path(args.qcf))
    adj_map = _list_ids(Path(args.adj))

    ids = set(tob_map) & set(qcf_map) & set(adj_map)
    if args.stations:
        if args.stations.startswith("@"):
            with open(args.stations[1:]) as fh:
                want = {ln.strip() for ln in fh if ln.strip()}
        else:
            want = {s.strip() for s in args.stations.split(",") if s.strip()}
        ids &= want
    ids = sorted(ids)

    if args.cutoff_from_adj:
        latest: Optional[Ym] = None
        for sid in ids:
            vals = ghcn_io.read_station_data(adj_map[sid]).values
            if vals:
                m = max(vals)
                latest = m if latest is None or m > latest else latest
        cutoff = latest if cutoff is None else min(cutoff, latest)

    acc = Acc()
    sink: Optional[list] = [] if args.per_station else None
    for sid in ids:
        tob = _values(tob_map[sid], cutoff)
        qcf = _values(qcf_map[sid], cutoff)
        adj = _values(adj_map[sid], cutoff)
        score_station(tob, qcf, adj, acc, sink, sid)

    if sink is not None:
        with open(args.per_station, "w") as fh:
            fh.write(
                "station_id\tcomposite\tdate\tlevel\tremove\t"
                "n_target\tn_matched\tn_missing\tn_extra\tn_removed\n"
            )
            for row in sorted(sink):
                fh.write("%s\t%.6f\t%.6f\t%.6f\t%.6f\t%d\t%d\t%d\t%d\t%d\n" % row)
        print("# per-station scores -> %s" % args.per_station)

    if acc.n == 0:
        print("# no stations scored (empty intersection of the three dirs)")
        return 1

    n = acc.n
    cut = f" (cutoff <= {cutoff[0]}-{cutoff[1]:02d})" if cutoff else ""
    print(f"# PHA-fit composite over {n} stations{cut}")
    print(
        f"#   weights: date={W_DATE} level={W_LEVEL} remove={W_REMOVE}; "
        f"level_tol={LEVEL_TOL} (hundredths C)"
    )
    print(f"composite_mean       {acc.sum_composite / n:.4f}")
    print(f"  date_mean          {acc.sum_date / n:.4f}")
    print(f"  level_mean         {acc.sum_level / n:.4f}")
    print(f"  remove_mean        {acc.sum_remove / n:.4f}")
    print(f"perfect_stations     {acc.n_perfect}/{n} ({100 * acc.n_perfect / n:.2f}%)")
    print("# transition dates (adjustment steps):")
    exact_rate = acc.t_level_exact / acc.t_matched if acc.t_matched else 1.0
    print(
        f"  target={acc.t_target} matched={acc.t_matched} "
        f"missing={acc.t_missing} extra={acc.t_extra} "
        f"level_exact_among_matched={exact_rate:.4f}"
    )
    if MIN_SEGMENT > 0 and (acc.imp_missed or acc.imp_invented):
        print(
            f"# short segments (< min-length={MIN_SEGMENT} observations) the two "
            f"series DISAGREE about:"
        )
        print(
            f"  target has, we do not: {acc.imp_missed}   "
            f"we have, target does not: {acc.imp_invented}   "
            f"(agreed and therefore ignored: {acc.imp_agreed})"
        )
        print(
            "  PHA emits short segments legitimately, so an agreed one is a\n"
            "  faithful reproduction. Only a disagreement bears on the fit."
        )
        for sid, gap, size in acc.imp_missed_stns[:5]:
            print(f"    missed   {sid} gap={gap}obs size={size / 100:.2f}C")
        for sid, gap, size in acc.imp_invented_stns[:5]:
            print(f"    invented {sid} gap={gap}obs size={size / 100:.2f}C")
    print("# data removals (months TOB has that QC dropped):")
    print(
        f"  qcf_removed={acc.r_target} matched={acc.r_matched} "
        f"missing={acc.r_missing} extra={acc.r_extra}"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
