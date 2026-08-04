"""TOB knowledge-gap report over one or more hint stores.

A CONUS station's TOB knowledge is the set of months inside its QCU data hull
that an evidenced regime accounts for.  Two measures are produced, because they
answer different questions:

*regime coverage* -- months spanned by a regime whose evidence class is
adoptable (``residual-proven``, ``residual-ambiguous``, ``residual-partial``,
plus the ``-hinted`` variants consolidation writes).  This is the structural
timeline: what the reconstruction is willing to assert for that month.

*constrained coverage* -- months that actually sat inside a constrained run of
some regime's evidence.  Strictly smaller than regime coverage (a regime spans
QCF holes it was never constrained over) and the honest answer to "which months
did the residual solve really pin down".

Consolidating many vintages can only grow both sets, so running this over one
vintage and then over the whole store measures the gap reduction the archive
buys.

Usage::

    uv run python src/python/tob_gap_report.py --hints data/intermediate/hints \\
        --out work/tob_gap_before.tsv
    uv run python src/python/tob_gap_report.py --hints /store/*/ \\
        --station-file work/conus_tob_gate_ids.txt --out work/tob_gap_report.tsv
"""

from __future__ import annotations

import argparse
import glob
import json
import os
import sys
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

ADOPTABLE = {
    "residual-proven",
    "residual-ambiguous",
    "residual-partial",
    "residual-proven-hinted",
    "residual-ambiguous-hinted",
    "residual-partial-hinted",
}

Month = int  # year * 12 + (month - 1), so month arithmetic is plain integers
Span = Tuple[Month, Month]


def to_month(ym: Sequence[int]) -> Month:
    return int(ym[0]) * 12 + (int(ym[1]) - 1)


def from_month(m: Month) -> str:
    return "%04d-%02d" % (m // 12, m % 12 + 1)


def merge(spans: Iterable[Span]) -> List[Span]:
    """Union of closed integer spans, sorted and coalesced (adjacent merged)."""
    out: List[Span] = []
    for lo, hi in sorted(spans):
        if hi < lo:
            continue
        if out and lo <= out[-1][1] + 1:
            if hi > out[-1][1]:
                out[-1] = (out[-1][0], hi)
        else:
            out.append((lo, hi))
    return out


def clip(spans: Iterable[Span], lo: Month, hi: Month) -> List[Span]:
    out: List[Span] = []
    for a, b in spans:
        a2, b2 = max(a, lo), min(b, hi)
        if a2 <= b2:
            out.append((a2, b2))
    return out


def complement(spans: Sequence[Span], lo: Month, hi: Month) -> List[Span]:
    """Months in [lo, hi] not covered by `spans` (which must be merged)."""
    out: List[Span] = []
    cur = lo
    for a, b in spans:
        if a > cur:
            out.append((cur, a - 1))
        cur = max(cur, b + 1)
        if cur > hi:
            break
    if cur <= hi:
        out.append((cur, hi))
    return out


def total(spans: Iterable[Span]) -> int:
    return sum(b - a + 1 for a, b in spans)


class StationEvidence:
    """Accumulated hull and coverage for one station across hint stores."""

    def __init__(self, sid: str) -> None:
        self.sid = sid
        self.qcu: Optional[Span] = None
        self.regime: List[Span] = []
        self.constrained: List[Span] = []
        self.vintages = 0
        self.codes: set = set()

    def add(self, doc: dict) -> None:
        hull = doc.get("qcu_hull")
        if hull:
            lo, hi = to_month(hull[0]), to_month(hull[1])
            # The QCU hull grows as vintages append months; take the union.
            self.qcu = (
                (lo, hi)
                if self.qcu is None
                else (min(self.qcu[0], lo), max(self.qcu[1], hi))
            )
        self.vintages += 1
        for r in doc.get("regimes") or []:
            ev = r.get("evidence") or {}
            if ev.get("class") not in ADOPTABLE:
                continue
            begin = r.get("begin")
            if not begin:
                continue
            b = to_month(begin)
            end = r.get("end")
            # An open-ended regime runs to the end of what this vintage saw; it
            # is clipped to the accumulated QCU hull in finish().
            e = to_month(end) if end else to_month(doc["qcu_hull"][1])
            self.regime.append((b, e))
            if r.get("code"):
                self.codes.add(r["code"])
            for run in ev.get("constrained_runs") or []:
                self.constrained.append((to_month(run[0]), to_month(run[1])))

    def finish(self) -> dict:
        if self.qcu is None:
            return {}
        lo, hi = self.qcu
        reg = merge(clip(merge(self.regime), lo, hi))
        con = merge(clip(merge(self.constrained), lo, hi))
        hull_n = hi - lo + 1
        rgaps = complement(reg, lo, hi)
        cgaps = complement(con, lo, hi)
        return {
            "station_id": self.sid,
            "vintages": self.vintages,
            "qcu_first": from_month(lo),
            "qcu_last": from_month(hi),
            "hull_months": hull_n,
            "regime_covered": total(reg),
            "regime_gap": total(rgaps),
            "constrained_covered": total(con),
            "constrained_gap": total(cgaps),
            "n_regime_gaps": len(rgaps),
            "n_codes": len(self.codes),
            "regime_gap_intervals": ";".join(
                "%s..%s" % (from_month(a), from_month(b)) for a, b in rgaps
            ),
        }


FIELDS = [
    "station_id",
    "vintages",
    "qcu_first",
    "qcu_last",
    "hull_months",
    "regime_covered",
    "regime_gap",
    "constrained_covered",
    "constrained_gap",
    "n_regime_gaps",
    "n_codes",
    "regime_gap_intervals",
]


def collect(
    hint_dirs: Sequence[str], wanted: Optional[set]
) -> Dict[str, StationEvidence]:
    acc: Dict[str, StationEvidence] = {}
    for d in hint_dirs:
        files = glob.glob(os.path.join(d, "*.hints.json"))
        if not files:
            print("warning: no hints in %s" % d, file=sys.stderr)
        for path in files:
            sid = os.path.basename(path).split(".")[0]
            if wanted is not None and sid not in wanted:
                continue
            try:
                with open(path) as fh:
                    doc = json.load(fh)
            except (OSError, ValueError) as exc:
                print("warning: unreadable %s (%s)" % (path, exc), file=sys.stderr)
                continue
            acc.setdefault(sid, StationEvidence(sid)).add(doc)
    return acc


def summarize(rows: Sequence[dict], n_dirs: int) -> List[str]:
    n = len(rows)
    full = sum(1 for r in rows if r["regime_gap"] == 0)
    part = sum(1 for r in rows if 0 < r["regime_gap"] < r["hull_months"])
    none = sum(1 for r in rows if r["regime_gap"] == r["hull_months"])
    hull_m = sum(r["hull_months"] for r in rows)
    reg_gap = sum(r["regime_gap"] for r in rows)
    con_gap = sum(r["constrained_gap"] for r in rows)
    return [
        "hint_dirs\t%d" % n_dirs,
        "stations\t%d" % n,
        "hull_months_total\t%d" % hull_m,
        "regime_gap_months_total\t%d" % reg_gap,
        "constrained_gap_months_total\t%d" % con_gap,
        "mean_hull_months\t%.2f" % (hull_m / n if n else 0.0),
        "mean_regime_gap_months\t%.3f" % (reg_gap / n if n else 0.0),
        "mean_constrained_gap_months\t%.3f" % (con_gap / n if n else 0.0),
        "regime_coverage_frac\t%.6f" % (1 - reg_gap / hull_m if hull_m else 0.0),
        "constrained_coverage_frac\t%.6f" % (1 - con_gap / hull_m if hull_m else 0.0),
        "stations_fully_covered\t%d" % full,
        "stations_partially_covered\t%d" % part,
        "stations_uncovered\t%d" % none,
    ]


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--hints", nargs="+", required=True, help="hint directories")
    ap.add_argument("--station-file", help="restrict to these station ids")
    ap.add_argument("--out", required=True, help="output TSV path")
    ap.add_argument("--summary", help="summary text path (default: <out>.summary)")
    args = ap.parse_args(argv)

    wanted = None
    if args.station_file:
        with open(args.station_file) as fh:
            wanted = {ln.strip() for ln in fh if ln.strip()}

    dirs = [d for d in args.hints if os.path.isdir(d)]
    if not dirs:
        sys.exit("no hint directories found among: %s" % " ".join(args.hints))
    acc = collect(dirs, wanted)

    rows = [r for r in (s.finish() for s in acc.values()) if r]
    rows.sort(key=lambda r: r["station_id"])
    with open(args.out, "w") as fh:
        fh.write("\t".join(FIELDS) + "\n")
        for r in rows:
            fh.write("\t".join(str(r[k]) for k in FIELDS) + "\n")

    lines = summarize(rows, len(dirs))
    out = args.summary or (args.out + ".summary")
    with open(out, "w") as fh:
        fh.write("\n".join(lines) + "\n")
    print("\n".join(lines))
    print("\nwrote %s (%d rows) and %s" % (args.out, len(rows), out))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
