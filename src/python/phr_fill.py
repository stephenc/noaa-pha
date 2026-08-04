#!/usr/bin/env python3
"""Full PHR fill: pre-, interior- and post-evidence, under strict precedence.

Precedence: **current vintage's own solve > donor hints > PHR**.  Expressed as
a per-month authority mask over the QCU hull:

  * ``vintage`` -- inside a constrained run of this vintage's residual
    evidence.  The solve pins the code, so PHR must not touch it.  This is
    also what preserves the bit-exact QCF identity ``verify_his`` enforces
    (``qcf == pha_qcf(t_out, S)`` for every month in a solved segment).
  * ``hint``    -- covered by a regime adopted from a donor vintage.
  * ``free``    -- neither; only here may PHR speak.

Every free region is filled: before, between and after the evidence.
``--regions pre`` restricts this to the leading edge alone.

## Why the solver needs no knowledge of metadata

Overwriting a code inside the QCF hull would break the exact reconstruction,
but the overlap is small: the great majority of interior PHR-documented data
months carry **no QCF value** (QC dropped them), and a month with no QCF value
cannot participate in the segment identity.  So the fill masks by authority and
*refuses* the residue rather than reasoning about it.  Three refusals keep the
reconstruction intact:

  1. **QCF-present months are never re-coded.**  Any candidate run containing
     one is dropped whole (not trimmed) -- trimming would leave a code flipping
     around a protected month, which is worse than not filling.
  2. **Runs containing an original regime begin are dropped.**  Those begins are
     solved transition dates; an override must never displace one.
  3. **Stations exceeding TOBMain's MAX_CHANGES (200) are left unfilled**, since
     the emitted file would be invalid.

Everything the fill refuses is counted and reported, so the residue is visible
rather than silent.

Usage:
    uv run python src/python/phr_fill.py --base <dir> [--regions pre,interior,post]
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from collections import Counter
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Set, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import ghcn_io  # noqa: E402
import his_emit  # noqa: E402
from metadata_accuracy import normalize_obs, resolve_obs  # noqa: E402

PAD_CODE = "00HR"  # midnight == zero adjustment: "undocumented", not "known"


def is_conus(sid: str, lat: float, lon: float) -> bool:
    """CONUS gate (mirrors tob_hints._is_conus / TOBMain's gate)."""
    return sid.startswith("US") and 24.0 <= lat <= 50.0 and -125.0 <= lon <= -66.0


Ym = Tuple[int, int]
Date = Tuple[int, int, int]


def mi(ym: Ym) -> int:
    return ym[0] * 12 + (ym[1] - 1)


def unmi(i: int) -> Ym:
    return (i // 12, i % 12 + 1)


def runs_of(months: Sequence[int]) -> List[Tuple[int, int]]:
    """Maximal contiguous runs from a sorted month-index iterable."""
    out: List[Tuple[int, int]] = []
    for m in sorted(months):
        if out and m == out[-1][1] + 1:
            out[-1] = (out[-1][0], m)
        else:
            out.append((m, m))
    return out


def phr_code_intervals(
    recs, lo: int, hi: int, tally: Counter
) -> List[Tuple[int, int, str]]:
    """PHR-documented (begin_mi, end_mi, code) intervals clipped to [lo, hi].

    Later-beginning records win overlaps.
    """
    periods = []
    for r in recs:
        if r.begin is None:
            continue
        codes, kind = normalize_obs(r.obs_time)
        tally[((r.obs_time or "").strip() or "(blank)", kind)] += 1
        if codes is None:
            continue
        code = resolve_obs(r.obs_time)
        if code is None:
            continue
        a = mi((r.begin[0], r.begin[1]))
        b = hi if r.end is None else mi((r.end[0], r.end[1]))
        periods.append((a, b, code))
    if not periods:
        return []
    periods.sort(key=lambda p: p[0])
    # Resolve to a per-month code (latest-beginning covering period wins).
    per: Dict[int, str] = {}
    for a, b, code in periods:
        for m in range(max(a, lo), min(b, hi) + 1):
            per[m] = code
    out: List[Tuple[int, int, str]] = []
    for m in sorted(per):
        if out and m == out[-1][1] + 1 and per[m] == out[-1][2]:
            out[-1] = (out[-1][0], m, out[-1][2])
        else:
            out.append((m, m, per[m]))
    return out


class StationFill:
    """Decide the override runs for one station."""

    def __init__(self, sid: str, sol: dict, raw, qcf, regions: Set[str]):
        self.sid = sid
        self.stats: Counter = Counter()
        self.regimes = sol.get("regimes") or []
        self.data = {
            mi(k) for k, v in raw.values.items() if v is not None and v > -9990
        }
        self.qcf_months = {
            mi(k)
            for k, v in (qcf.values if qcf else {}).items()
            if v is not None and v > -9990
        }
        self.lo, self.hi = min(self.data), max(self.data)
        self.regions = regions

        # --- authority -----------------------------------------------------
        self.vintage: Set[int] = set()
        for er in (sol.get("evidence") or {}).get("regimes", []):
            for a, b in er.get("constrained_runs", []):
                self.vintage.update(range(mi((a[0], a[1])), mi((b[0], b[1])) + 1))
        self.vintage &= set(range(self.lo, self.hi + 1))

        self.hint: Set[int] = set()
        for k, r in enumerate(self.regimes):
            if str(r.get("source", "residual")).startswith("hint"):
                a = mi((r["begin"][0], r["begin"][1]))
                b = (
                    mi(
                        (
                            self.regimes[k + 1]["begin"][0],
                            self.regimes[k + 1]["begin"][1],
                        )
                    )
                    - 1
                    if k + 1 < len(self.regimes)
                    else self.hi
                )
                self.hint.update(range(max(a, self.lo), min(b, self.hi) + 1))
        self.hint -= self.vintage

        self.auth = self.vintage | self.hint
        self.free = set(range(self.lo, self.hi + 1)) - self.auth
        self.env_lo = min(self.auth) if self.auth else None
        self.env_hi = max(self.auth) if self.auth else None

        # Original regime begin months -- solved transitions, never displaced.
        self.begin_months = {mi((r["begin"][0], r["begin"][1])) for r in self.regimes}

        # Base (pre-fill) code **on day 1 of each month**, from the contiguous
        # consolidated timeline; months before the first regime carry the pad.
        #
        # Day-1 semantics are load-bearing.  A regime beginning mid-month (e.g.
        # 2014-07-02) does NOT own day 1 of that month: TOBMain blends the month
        # across the two codes.  Treating it as owning the whole month makes the
        # restore row we insert at (month, 1) carry the *new* code, which erases
        # the blend and silently changes a month the solve had pinned.
        self.base_code: Dict[int, str] = {}
        cur = PAD_CODE
        idx = 0
        for m in range(self.lo, self.hi + 1):
            while idx < len(self.regimes):
                by, bm, bd = self.regimes[idx]["begin"]
                # effective from day 1 of its own month only if it begins there
                eff = mi((by, bm)) if bd == 1 else mi((by, bm)) + 1
                if eff > m:
                    break
                cur = self.regimes[idx]["code"]
                idx += 1
            self.base_code[m] = cur

    def region_of(self, m: int) -> str:
        if self.env_lo is None:
            return "interior"
        if m < self.env_lo:
            return "pre"
        if m > self.env_hi:
            return "post"
        return "interior"

    def overrides(
        self, intervals: List[Tuple[int, int, str]]
    ) -> List[Tuple[int, int, str]]:
        """Accepted (begin_mi, end_mi, code) override runs."""
        accepted: List[Tuple[int, int, str]] = []
        for a, b, code in intervals:
            cand = [m for m in range(a, b + 1) if m in self.free]
            for ra, rb in runs_of(cand):
                months = range(ra, rb + 1)
                region = self.region_of(ra)
                if region not in self.regions:
                    continue
                if all(self.base_code.get(m) == code for m in months):
                    self.stats["skip_already_equal"] += 1
                    continue
                if any(m in self.qcf_months for m in months):
                    self.stats["refuse_qcf_present"] += 1
                    self.stats["refuse_qcf_present_months"] += rb - ra + 1
                    continue
                if any(m in self.begin_months for m in months):
                    self.stats["refuse_solved_begin"] += 1
                    continue
                accepted.append((ra, rb, code))
                self.stats["accept_" + region] += 1
                self.stats["accept_months_" + region] += rb - ra + 1
                self.stats["accept_data_months_" + region] += sum(
                    1 for m in months if m in self.data
                )
        return accepted

    def emit_regimes(
        self, accepted: List[Tuple[int, int, str]]
    ) -> Optional[List[his_emit.Regime]]:
        """Merge accepted overrides into the original regime rows."""
        if not accepted:
            return None
        rows: Dict[Date, str] = {}
        for r in self.regimes:
            rows[tuple(r["begin"])] = r["code"]
        for a, b, code in accepted:
            ya, ma = unmi(a)
            rows[(ya, ma, 1)] = code
            if b + 1 <= self.hi:
                yb, mb = unmi(b + 1)
                restore = self.base_code.get(b + 1, PAD_CODE)
                # An original row may already start exactly here; leave it.
                rows.setdefault((yb, mb, 1), restore)
        ordered = sorted(rows.items())
        # Collapse consecutive equal codes.
        out: List[his_emit.Regime] = []
        prev = None
        for date, code in ordered:
            if code != prev:
                out.append(his_emit.Regime(begin=date, obs_time=code))
                prev = code
        if len(out) > his_emit.MAX_CODE_CHANGES:
            self.stats["refuse_max_changes"] += 1
            return None
        return out


REPORT_HEADER = (
    "station_id\thull\tfree\tpre_months\tinterior_months\tpost_months"
    "\tdata_months_filled\tn_rows\trefused_qcf\trefused_begin"
)


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--base", required=True)
    ap.add_argument("--phr-zip", default=None)
    ap.add_argument("--regions", default="pre,interior,post")
    ap.add_argument("--report", default=None)
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args(argv)

    regions = {r.strip() for r in args.regions.split(",") if r.strip()}
    base = Path(args.base)
    sol_dir = base / "intermediate" / "solutions"
    raw_dir = base / "input" / "raw" / "tavg"
    qcf_dir = base / "output" / "qcf" / "tavg"
    his_dir = base / "intermediate" / "history"
    inv = ghcn_io.read_inventory(base / "intermediate" / "station.inv")
    phr_zip = Path(args.phr_zip) if args.phr_zip else base / "phr.txt.zip"

    sids = sorted(p.name[: -len(".json")] for p in sol_dir.glob("*.json"))
    cand = [s for s in sids if s in inv and is_conus(s, inv[s].lat, inv[s].lon)]
    phr = ghcn_io.read_phr(phr_zip, set(cand))
    print("# regions=%s  CONUS=%d  PHR=%d" % (sorted(regions), len(cand), len(phr)))

    tally: Counter = Counter()
    tot: Counter = Counter()
    rows: List[str] = []
    n_written = 0

    for sid in cand:
        with open(sol_dir / f"{sid}.json") as fh:
            sol = json.load(fh)
        if sol.get("kind") != "tob" or not (sol.get("regimes") or []):
            continue
        raw_p = raw_dir / f"{sid}.raw.tavg"
        if not raw_p.exists():
            continue
        recs = phr.get(sid) or []
        if not recs:
            continue
        raw = ghcn_io.read_station_data(raw_p)
        if not raw.values:
            continue
        qcf_p = qcf_dir / f"{sid}.qcf.tavg"
        qcf = ghcn_io.read_station_data(qcf_p) if qcf_p.exists() else None

        try:
            st = StationFill(sid, sol, raw, qcf, regions)
        except ValueError:
            continue
        intervals = phr_code_intervals(recs, st.lo, st.hi, tally)
        if not intervals:
            continue
        accepted = st.overrides(intervals)
        regimes = st.emit_regimes(accepted)
        for k, v in st.stats.items():
            tot[k] += v
        if regimes is None:
            continue

        n_written += 1
        filled_data = sum(
            st.stats.get("accept_data_months_" + r, 0)
            for r in ("pre", "interior", "post")
        )
        tot["stations_filled"] += 1
        tot["data_months_filled"] += filled_data
        rows.append(
            "%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d"
            % (
                sid,
                st.hi - st.lo + 1,
                len(st.free),
                st.stats.get("accept_months_pre", 0),
                st.stats.get("accept_months_interior", 0),
                st.stats.get("accept_months_post", 0),
                filled_data,
                len(regimes),
                st.stats.get("refuse_qcf_present", 0),
                st.stats.get("refuse_solved_begin", 0),
            )
        )
        if args.dry_run:
            continue
        rec = inv[sid]
        coord = sol.get("coord") or [rec.lat, rec.lon]
        dms = ghcn_io.dms_quantize(coord[0], coord[1])
        first_year = raw.years[0] if raw.years else 1895
        his_emit.emit_station_his(sid, regimes, dms, rec, his_dir, first_year)

    out = [
        "# --- PHR fill (regions=%s, dry_run=%s) ---"
        % (",".join(sorted(regions)), args.dry_run)
    ]
    for k in sorted(tot):
        out.append("%s\t%d" % (k, tot[k]))
    text = "\n".join(out)
    print(text)

    if args.report:
        with open(args.report, "w") as fh:
            fh.write(REPORT_HEADER + "\n")
            for r in rows:
                fh.write(r + "\n")
        with open(args.report + ".summary", "w") as fh:
            fh.write(text + "\n")
    print("# histories written: %d" % (0 if args.dry_run else n_written))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
