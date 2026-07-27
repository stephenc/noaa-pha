"""Quantify PHR observation-time metadata accuracy against the RECOVERED
TOB change timeline.

The recovered timeline (solution JSONs, exact residual decomposition) is
treated as ground truth; each verifiable PHR-documented obs-time change is
scored against it.  Hypothesis under test: metadata is only ~70-80%
accurate, though in principle it should be correct at least to the month.

Sources: PHR (TIME_OF_OBS) is the PRIMARY and only obs-time source used.
The enhanced MSHR file carries no time-of-observation column in the slices
we parse, so --mshr-zip is accepted but used only as documentation of that
decision (see report header).

Usage:
  uv run python src/python/metadata_accuracy.py \
      --solutions data/intermediate/solutions --phr-zip data/phr.txt.zip \
      --raw-dir data/input/raw/tavg --report work/metadata_accuracy.tsv
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, FrozenSet, List, Optional, Tuple

sys.path.insert(0, str(Path(__file__).resolve().parent))

import ghcn_io  # noqa: E402

MATCH_WINDOW_MONTHS = 2
DAY_TOL = 2
GAP_MONTHS = 3  # metadata events inside a >=3-month data hole are unverifiable

# ---------------------------------------------------------------------------
# TIME_OF_OBS normalization
# ---------------------------------------------------------------------------

SENTINELS = {"9999", "8888", "7777", "7070", "6666", "5555", "4444", "3030"}
SPECIALS = {
    "SS": "00SS",
    "SR": "00SR",
    "RS": "00RS",
    "TRID": "TRID",
    # Per the official PHR_Table.txt "Special TIME_OF_OBS values" list
    # (data/PHR_Table.txt): 0630 and 1830 are SENTINELS meaning sunrise and
    # sunset, NOT literal half-hour clock times.
    "0630": "00SR",
    "1830": "00SS",
}


def normalize_obs(raw: Optional[str]) -> Tuple[Optional[FrozenSet[str]], str]:
    """Normalize a raw PHR TIME_OF_OBS to our code set.

    Returns (codes, kind): codes is a frozenset of acceptable 4-char codes
    (half-hour times map to BOTH bracketing hours -- we do not guess which
    way NOAA rounded), or None when unknown.  kind is one of
    'hour' | 'half-hour' | 'special' | 'unknown' | 'unmappable'.
    """
    if raw is None:
        return None, "unknown"
    v = raw.strip().upper()
    if not v or v in SENTINELS or v in {"UNKN", "VAR"}:
        return None, "unknown"
    if v in SPECIALS:
        return frozenset({SPECIALS[v]}), "special"
    if v.isdigit() and len(v) == 4:
        hh, mm = int(v[:2]), int(v[2:])
        if hh > 24 or mm > 59:
            return None, "unmappable"
        if hh in (0, 24):
            hh = 24
        if mm == 0:
            return frozenset({f"{hh:02d}HR"}), "hour"
        # half-hour (e.g. 0730): we do not guess NOAA's rounding direction --
        # both bracketing hourly codes are acceptable matches
        lo = 24 if hh == 0 else hh
        hi = 1 if hh in (0, 24) else hh + 1
        return frozenset({f"{lo:02d}HR", f"{hi:02d}HR"}), "half-hour"
    return None, "unmappable"


# ---------------------------------------------------------------------------
# Event extraction
# ---------------------------------------------------------------------------


@dataclass
class MetaEvent:
    date: Tuple[int, int, int]
    codes_before: Optional[FrozenSet[str]]
    codes_after: Optional[FrozenSet[str]]
    raw_before: str
    raw_after: str
    uncertain_timing: bool  # unknown-coded period(s) intervene


@dataclass
class RecEvent:
    date: Tuple[int, int, int]
    day_resolved: bool
    code_before: str
    code_after: str


def meta_events(recs: List[ghcn_io.PhrRec]) -> Tuple[List[MetaEvent], Counter]:
    """Change events from PHR periods (chronological, dated at the later
    period's begin).  Transitions to/from unknown-coded periods produce an
    event only between the flanking KNOWN periods, flagged uncertain."""
    mapping = Counter()
    periods = []
    for r in sorted(recs, key=lambda r: r.begin or (0, 0, 0)):
        if r.begin is None:
            continue
        codes, kind = normalize_obs(r.obs_time)
        mapping[(r.obs_time or "").strip() or "(blank)", kind] += 1
        periods.append((r.begin, codes, (r.obs_time or "").strip()))
    events: List[MetaEvent] = []
    prev_known = None  # (begin, codes, raw)
    pending_unknown = False
    for begin, codes, raw in periods:
        if codes is None:
            pending_unknown = prev_known is not None
            continue
        if prev_known is not None and codes != prev_known[1]:
            events.append(
                MetaEvent(
                    date=begin,
                    codes_before=prev_known[1],
                    codes_after=codes,
                    raw_before=prev_known[2],
                    raw_after=raw,
                    uncertain_timing=pending_unknown,
                )
            )
        prev_known = (begin, codes, raw)
        pending_unknown = False
    return events, mapping


def recovered_events(sol: dict) -> List[RecEvent]:
    """Regime transitions after the first regime.

    Note on the emitter's leading-00HR bracketing row: solution JSONs never
    contain it (his_emit prepends it at write time only), so no structural
    exclusion is needed here; a leading 24HR regime in a solution is a
    genuine solved no-TOB era and the transition out of it is a real
    recovered event.
    """
    regimes = sol.get("regimes") or []
    out: List[RecEvent] = []
    for k in range(1, len(regimes)):
        r = regimes[k]
        y, m, d = r["begin"]
        out.append(
            RecEvent(
                date=(y, m, d),
                day_resolved=r.get("blend_day") is not None or d > 1,
                code_before=regimes[k - 1]["code"],
                code_after=r["code"],
            )
        )
    return out


# ---------------------------------------------------------------------------
# Verifiability
# ---------------------------------------------------------------------------


def _mi(y: int, m: int) -> int:
    return y * 12 + (m - 1)


def unverifiable_reason(
    ev: MetaEvent,
    raw_months: set,
    has_tob_solution: bool,
    regimes: Optional[List[dict]] = None,
) -> Optional[str]:
    if not has_tob_solution:
        return "no-tob-solution"
    if not raw_months:
        return "no-data"
    y, m, _d = ev.date
    mi = _mi(y, m)
    # A change during a recovered no-TOB era (24HR regime, adjustment == 0)
    # is invisible to the residuals: an obs-time change there produces no
    # signal, so it cannot be verified or falsified.
    if regimes:
        active = None
        for r in regimes:
            if (r["begin"][0], r["begin"][1]) <= (y, m):
                active = r["code"]
        if active == "24HR":
            return "no-tob-era"
    lo, hi = min(raw_months), max(raw_months)
    if mi < lo:
        return "before-record"
    if mi > hi:
        return "after-record"
    if mi not in raw_months:
        run = 0
        k = mi
        while k >= lo and k not in raw_months:
            run += 1
            k -= 1
        k = mi + 1
        while k <= hi and k not in raw_months:
            run += 1
            k += 1
        if run >= GAP_MONTHS:
            return "in-data-gap"
    if ev.uncertain_timing:
        return "unknown-period-adjacent"
    return None


# ---------------------------------------------------------------------------
# Matching + classification
# ---------------------------------------------------------------------------


@dataclass
class StationScore:
    sid: str
    exact: bool
    n_meta: int = 0
    n_verifiable: int = 0
    n_recovered: int = 0
    code_day: int = 0
    code_month: int = 0
    code_near: int = 0  # code correct, 1-2 months off (not in-month)
    time_only_day: int = 0
    time_only_month: int = 0
    missed: int = 0
    undocumented: int = 0
    unverifiable: int = 0
    confusion: Counter = field(default_factory=Counter)

    def tsv(self) -> str:
        return (
            f"{self.sid}\t{self.exact}\t{self.n_meta}\t{self.n_verifiable}"
            f"\t{self.n_recovered}\t{self.code_day}\t{self.code_month}"
            f"\t{self.code_near}\t{self.time_only_day}\t{self.time_only_month}"
            f"\t{self.missed}\t{self.undocumented}\t{self.unverifiable}"
        )


TSV_HEADER = (
    "station\texact\tn_meta\tn_verifiable\tn_recovered\tcode_day\tcode_month"
    "\tcode_near\ttime_only_day\ttime_only_month\tmissed\tundocumented"
    "\tunverifiable"
)


def score_station(
    sid: str,
    sol: Optional[dict],
    phr_recs: List[ghcn_io.PhrRec],
    raw_months: set,
) -> Tuple[StationScore, Counter]:
    events, mapping = meta_events(phr_recs)
    has_tob = bool(sol) and sol.get("kind") == "tob" and sol.get("regimes")
    recs = recovered_events(sol) if has_tob else []
    sc = StationScore(
        sid=sid,
        exact=bool(sol) and bool(sol.get("exact")),
        n_meta=len(events),
        n_recovered=len(recs),
    )

    verifiable: List[MetaEvent] = []
    for ev in events:
        reason = unverifiable_reason(
            ev, raw_months, has_tob, sol.get("regimes") if has_tob else None
        )
        if reason is None:
            verifiable.append(ev)
        else:
            sc.unverifiable += 1
    sc.n_verifiable = len(verifiable)

    # greedy nearest-first one-to-one matching within the window
    cands = []
    for i, ev in enumerate(verifiable):
        for j, rc in enumerate(recs):
            dm = abs(_mi(ev.date[0], ev.date[1]) - _mi(rc.date[0], rc.date[1]))
            if dm <= MATCH_WINDOW_MONTHS:
                cands.append((dm, abs(ev.date[2] - rc.date[2]), i, j))
    cands.sort()
    used_meta: set = set()
    used_rec: set = set()
    pairs: Dict[int, int] = {}
    for _dm, _dd, i, j in cands:
        if i in used_meta or j in used_rec:
            continue
        used_meta.add(i)
        used_rec.add(j)
        pairs[i] = j

    for i, ev in enumerate(verifiable):
        j = pairs.get(i)
        if j is None:
            sc.missed += 1
            continue
        rc = recs[j]
        code_ok = ev.codes_after is not None and rc.code_after in ev.codes_after
        same_month = (ev.date[0], ev.date[1]) == (rc.date[0], rc.date[1])
        day_ok = same_month and abs(ev.date[2] - rc.date[2]) <= DAY_TOL
        if code_ok and day_ok:
            sc.code_day += 1
        elif code_ok and same_month:
            sc.code_month += 1
        elif code_ok:
            sc.code_near += 1
        else:
            meta_code = "/".join(sorted(ev.codes_after or {"?"}))
            sc.confusion[(meta_code, rc.code_after)] += 1
            if day_ok:
                sc.time_only_day += 1
            else:
                sc.time_only_month += 1
    sc.undocumented = len(recs) - len(used_rec)
    return sc, mapping


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--solutions", required=True)
    ap.add_argument("--phr-zip", required=True)
    ap.add_argument(
        "--mshr-zip",
        default=None,
        help="accepted; NOT used (no obs-time in parsed MSHR slices)",
    )
    ap.add_argument("--raw-dir", required=True)
    ap.add_argument("--report", required=True)
    ap.add_argument("--stations", default=None)
    args = ap.parse_args()

    sol_dir = Path(args.solutions)
    raw_dir = Path(args.raw_dir)
    if args.stations:
        sids = [s.strip() for s in args.stations.split(",") if s.strip()]
    else:
        sids = sorted(p.stem for p in sol_dir.glob("*.json"))
    print(
        f"# scoring {len(sids)} stations with solutions present "
        "(in-progress solution sets are scored as-is)"
    )

    phr = ghcn_io.read_phr(Path(args.phr_zip), set(sids))

    scores: List[StationScore] = []
    mapping_total: Counter = Counter()
    confusion_total: Counter = Counter()
    for sid in sids:
        try:
            sol = json.load(open(sol_dir / f"{sid}.json"))
        except Exception:
            sol = None
        raw_path = raw_dir / f"{sid}.raw.tavg"
        raw_months: set = set()
        if raw_path.exists():
            sd = ghcn_io.read_station_data(raw_path)
            raw_months = {_mi(y, m) for (y, m) in sd.values}
        sc, mapping = score_station(sid, sol, phr.get(sid, []), raw_months)
        mapping_total.update(mapping)
        confusion_total.update(sc.confusion)
        scores.append(sc)

    report = Path(args.report)
    report.parent.mkdir(parents=True, exist_ok=True)
    with open(report, "w") as fh:
        fh.write(TSV_HEADER + "\n")
        for sc in scores:
            fh.write(sc.tsv() + "\n")

    def aggregate(subset: List[StationScore], label: str) -> None:
        v = sum(s.n_verifiable for s in subset)
        if v == 0:
            print(f"[{label}] no verifiable metadata events")
            return
        cd = sum(s.code_day for s in subset)
        cm = sum(s.code_month for s in subset)
        cn = sum(s.code_near for s in subset)
        td = sum(s.time_only_day for s in subset)
        tm = sum(s.time_only_month for s in subset)
        ms = sum(s.missed for s in subset)
        ud = sum(s.undocumented for s in subset)
        uv = sum(s.unverifiable for s in subset)
        print(
            f"[{label}] verifiable meta events: {v} "
            f"(+{uv} unverifiable, {ud} undocumented recovered changes)\n"
            f"  code+day       {cd:5d}  {100*cd/v:5.1f}%\n"
            f"  code+month     {cm:5d}  {100*cm/v:5.1f}%\n"
            f"  code near(1-2mo){cn:4d}  {100*cn/v:5.1f}%\n"
            f"  wrong-code@day {td:5d}  {100*td/v:5.1f}%\n"
            f"  wrong-code@mon {tm:5d}  {100*tm/v:5.1f}%\n"
            f"  missed         {ms:5d}  {100*ms/v:5.1f}%\n"
            f"  => code correct to month or better: {100*(cd+cm)/v:5.1f}%  "
            f"| any correct within 2mo: {100*(cd+cm+cn)/v:5.1f}%"
        )

    aggregate(scores, "ALL")
    aggregate([s for s in scores if s.exact], "EXACT-ONLY")

    print("\nTIME_OF_OBS mapping table (raw value, kind, count):")
    for (raw, kind), c in mapping_total.most_common(30):
        print(f"  {raw:>8} {kind:>10} {c}")
    n_unmap = sum(c for (r, k), c in mapping_total.items() if k == "unmappable")
    print(f"  unmappable total: {n_unmap}")

    print("\nCode-confusion pairs (metadata -> recovered, count):")
    for (mc, rc), c in confusion_total.most_common(25):
        print(f"  {mc:>10} -> {rc}  {c}")


if __name__ == "__main__":
    main()
