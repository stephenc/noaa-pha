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


def _transitions(adj: Dict[Ym, int]) -> Dict[Ym, int]:
    """Map {month -> level} at each level CHANGE, walking present months in
    time order.  A transition month is the first month of a new level; its
    value is the new level.  The earliest present month is the baseline, not
    a transition."""
    out: Dict[Ym, int] = {}
    prev: Optional[int] = None
    for ym in sorted(adj):
        lvl = adj[ym]
        if prev is not None and lvl != prev:
            out[ym] = lvl
        prev = lvl
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
        # corpus-wide removal counts
        self.r_target = 0
        self.r_matched = 0
        self.r_missing = 0
        self.r_extra = 0
        # perfect stations (composite == 1.0)
        self.n_perfect = 0


def score_station(
    tob: Dict[Ym, int], qcf: Dict[Ym, int], adj: Dict[Ym, int], acc: Acc
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

    acc.r_target += len(r_qcf)
    acc.r_matched += len(r_qcf & r_pha)
    acc.r_missing += len(r_qcf - r_pha)
    acc.r_extra += len(r_pha - r_qcf)


def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--tob", required=True, help="TOB baseline dir (*.tob.tavg)")
    ap.add_argument("--qcf", required=True, help="published QCF dir (target)")
    ap.add_argument("--adj", required=True, help="our PHA output dir (adj)")
    ap.add_argument(
        "--stations", default=None, help="comma-separated subset of station ids"
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
    return ap.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> int:
    args = parse_args(argv)
    cutoff = _parse_cutoff(args.cutoff)
    tob_map = _list_ids(Path(args.tob))
    qcf_map = _list_ids(Path(args.qcf))
    adj_map = _list_ids(Path(args.adj))

    ids = set(tob_map) & set(qcf_map) & set(adj_map)
    if args.stations:
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
    for sid in ids:
        tob = _values(tob_map[sid], cutoff)
        qcf = _values(qcf_map[sid], cutoff)
        adj = _values(adj_map[sid], cutoff)
        score_station(tob, qcf, adj, acc)

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
    print("# data removals (months TOB has that QC dropped):")
    print(
        f"  qcf_removed={acc.r_target} matched={acc.r_matched} "
        f"missing={acc.r_missing} extra={acc.r_extra}"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
