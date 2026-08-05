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


def read_raw_months(path: str) -> set:
    """Months carrying an actual QCU value in a PHA raw file.

    Fixed-width: 11-char id, space, 4-char year, then 12 x 9-char fields whose
    first 6 chars are the value ("-9999" means missing).  The hull span counts
    holes as months; this counts only months with data, which is what "uncovered
    QCU data point" means.
    """
    out: set = set()
    with open(path, "r", errors="replace") as fh:
        for line in fh:
            if len(line) < 20:
                continue
            try:
                year = int(line[12:16])
            except ValueError:
                continue
            for m in range(12):
                off = 16 + m * 9
                v = line[off : off + 6].strip()
                if not v or v == "-9999":
                    continue
                out.add(year * 12 + m)
    return out


def covered_count(months: set, spans: Sequence[Span]) -> int:
    """How many of *months* fall inside the merged *spans*."""
    n = 0
    for m in months:
        lo, hi = 0, len(spans) - 1
        while lo <= hi:
            mid = (lo + hi) // 2
            a, b = spans[mid]
            if m < a:
                hi = mid - 1
            elif m > b:
                lo = mid + 1
            else:
                n += 1
                break
    return n


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

    def finish(
        self,
        raw_dir: Optional[str] = None,
        phr_spans: Optional[Sequence[Span]] = None,
    ) -> dict:
        if self.qcu is None:
            return {}
        lo, hi = self.qcu
        reg = merge(clip(merge(self.regime), lo, hi))
        con = merge(clip(merge(self.constrained), lo, hi))
        hull_n = hi - lo + 1
        rgaps = complement(reg, lo, hi)
        cgaps = complement(con, lo, hi)
        extra = {}
        if raw_dir is not None:
            path = os.path.join(raw_dir, "%s.raw.tavg" % self.sid)
            months = read_raw_months(path) if os.path.isfile(path) else set()
            ndata = len(months)
            rcov = covered_count(months, reg)
            ccov = covered_count(months, con)
            extra = {
                "qcu_data_months": ndata,
                "data_regime_covered": rcov,
                "data_regime_uncovered": ndata - rcov,
                "data_constrained_uncovered": ndata - ccov,
            }
            if phr_spans is not None:
                # Of the months the residual evidence cannot reach, how many are
                # inside a PHR-documented observation-time span?  This measures
                # only availability of documentation -- PHR remains a search
                # hint, never a constraint.
                unc = {m for m in months if not covered_count({m}, reg)}
                uncon = {m for m in months if not covered_count({m}, con)}
                extra["phr_fillable_regime"] = covered_count(unc, phr_spans)
                extra["phr_fillable_constrained"] = covered_count(uncon, phr_spans)
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
            **extra,
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

# Present only with --qcu-raw: months that actually carry a QCU value, rather
# than every month in the hull span.
DATA_FIELDS = [
    "qcu_data_months",
    "data_regime_covered",
    "data_regime_uncovered",
    "data_constrained_uncovered",
]

# Present only with --phr-zip.
PHR_FIELDS = ["phr_fillable_regime", "phr_fillable_constrained"]


def phr_month_spans(recs: Sequence, hull_hi: Month) -> List[Span]:
    """Month spans over which PHR documents an observation time.

    A record with no TIME_OF_OBS documents nothing.  An open end runs to the end
    of the station's data.
    """
    spans: List[Span] = []
    for r in recs:
        if getattr(r, "obs_time", None) is None or r.begin is None:
            continue
        b = to_month(r.begin[:2])
        e = to_month(r.end[:2]) if r.end else hull_hi
        if e >= b:
            spans.append((b, e))
    return merge(spans)


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
    ] + _summarize_data(rows)


def _summarize_data(rows: Sequence[dict]) -> List[str]:
    """Uncovered *actual QCU data* months (only with --qcu-raw)."""
    rows = [r for r in rows if "qcu_data_months" in r]
    if not rows:
        return []
    n = len(rows)
    data_m = sum(r["qcu_data_months"] for r in rows)
    unc = sum(r["data_regime_uncovered"] for r in rows)
    cunc = sum(r["data_constrained_uncovered"] for r in rows)
    incomplete = [r for r in rows if r["data_regime_uncovered"] > 0]
    cincomplete = [r for r in rows if r["data_constrained_uncovered"] > 0]
    worst = max(rows, key=lambda r: r["data_regime_uncovered"])
    return [
        "",
        "# --- uncovered ACTUAL QCU data months (holes excluded) ---",
        "qcu_data_months_total\t%d" % data_m,
        "data_regime_uncovered_total\t%d" % unc,
        "data_constrained_uncovered_total\t%d" % cunc,
        "data_regime_coverage_frac\t%.6f" % (1 - unc / data_m if data_m else 0.0),
        "data_constrained_coverage_frac\t%.6f" % (1 - cunc / data_m if data_m else 0.0),
        "stations_with_uncovered_data\t%d" % len(incomplete),
        "stations_with_uncovered_data_frac\t%.6f" % (len(incomplete) / n),
        "stations_with_unconstrained_data\t%d" % len(cincomplete),
        "stations_with_unconstrained_data_frac\t%.6f" % (len(cincomplete) / n),
        "mean_uncovered_months_incomplete_only\t%.3f"
        % (unc / len(incomplete) if incomplete else 0.0),
        "median_uncovered_months_incomplete_only\t%d"
        % (
            sorted(r["data_regime_uncovered"] for r in incomplete)[len(incomplete) // 2]
            if incomplete
            else 0
        ),
        "mean_unconstrained_months_incomplete_only\t%.3f"
        % (cunc / len(cincomplete) if cincomplete else 0.0),
        "worst_station\t%s" % worst["station_id"],
        "worst_station_uncovered_months\t%d" % worst["data_regime_uncovered"],
        "worst_station_qcu_data_months\t%d" % worst["qcu_data_months"],
        "worst_station_hull\t%s..%s" % (worst["qcu_first"], worst["qcu_last"]),
    ] + _summarize_phr(rows, unc, cunc)


def _summarize_phr(rows: Sequence[dict], unc: int, cunc: int) -> List[str]:
    """How much of the uncovered gap has PHR documentation available."""
    rows = [r for r in rows if "phr_fillable_regime" in r]
    if not rows:
        return []
    fr = sum(r["phr_fillable_regime"] for r in rows)
    fc = sum(r["phr_fillable_constrained"] for r in rows)
    helped = sum(1 for r in rows if r["phr_fillable_regime"] > 0)
    closed = sum(
        1
        for r in rows
        if r["data_regime_uncovered"] > 0
        and r["phr_fillable_regime"] == r["data_regime_uncovered"]
    )
    return [
        "",
        "# --- PHR documentation available over the uncovered gap ---",
        "phr_fillable_regime_months\t%d" % fr,
        "phr_fillable_regime_frac_of_gap\t%.6f" % (fr / unc if unc else 0.0),
        "phr_fillable_constrained_months\t%d" % fc,
        "phr_fillable_constrained_frac_of_gap\t%.6f" % (fc / cunc if cunc else 0.0),
        "stations_with_any_phr_fillable\t%d" % helped,
        "stations_gap_fully_documented\t%d" % closed,
    ]


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--hints", nargs="+", required=True, help="hint directories")
    ap.add_argument("--station-file", help="restrict to these station ids")
    ap.add_argument("--out", required=True, help="output TSV path")
    ap.add_argument("--summary", help="summary text path (default: <out>.summary)")
    ap.add_argument(
        "--qcu-raw",
        metavar="DIR",
        help="PHA raw QCU dir (<base>/input/raw/tavg). With this, coverage is "
        "also measured against months that actually carry a QCU value, rather "
        "than every month in the hull span",
    )
    ap.add_argument(
        "--phr-zip",
        metavar="ZIP",
        help="PHR zip. Reports how many otherwise-uncovered QCU data months sit "
        "inside a PHR-documented observation-time span (availability only -- PHR "
        "stays a search hint, never a constraint). Requires --qcu-raw",
    )
    args = ap.parse_args(argv)
    if args.qcu_raw is not None and not os.path.isdir(args.qcu_raw):
        sys.exit("--qcu-raw: no such directory: %s" % args.qcu_raw)
    if args.phr_zip is not None:
        if not os.path.isfile(args.phr_zip):
            sys.exit("--phr-zip: no such file: %s" % args.phr_zip)
        if args.qcu_raw is None:
            sys.exit("--phr-zip requires --qcu-raw (it measures data months)")

    wanted = None
    if args.station_file:
        with open(args.station_file) as fh:
            wanted = {ln.strip() for ln in fh if ln.strip()}

    dirs = [d for d in args.hints if os.path.isdir(d)]
    if not dirs:
        sys.exit("no hint directories found among: %s" % " ".join(args.hints))
    acc = collect(dirs, wanted)

    phr: Dict[str, list] = {}
    if args.phr_zip:
        sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
        import ghcn_io  # noqa: E402  (repo-local module)

        phr = ghcn_io.read_phr(__import__("pathlib").Path(args.phr_zip), set(acc))
        print("phr: obs-time records for %d/%d stations" % (len(phr), len(acc)))

    rows = []
    for s in acc.values():
        spans = None
        if args.phr_zip:
            spans = phr_month_spans(phr.get(s.sid, []), s.qcu[1] if s.qcu else 0)
        r = s.finish(args.qcu_raw, spans)
        if r:
            rows.append(r)
    rows.sort(key=lambda r: r["station_id"])
    fields = FIELDS + (DATA_FIELDS if args.qcu_raw else [])
    if args.phr_zip:
        fields = fields + PHR_FIELDS
    with open(args.out, "w") as fh:
        fh.write("\t".join(fields) + "\n")
        for r in rows:
            fh.write("\t".join(str(r[k]) for k in fields) + "\n")

    lines = summarize(rows, len(dirs))
    out = args.summary or (args.out + ".summary")
    with open(out, "w") as fh:
        fh.write("\n".join(lines) + "\n")
    print("\n".join(lines))
    print("\nwrote %s (%d rows) and %s" % (args.out, len(rows), out))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
