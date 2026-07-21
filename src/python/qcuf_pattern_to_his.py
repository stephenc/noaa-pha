#!/usr/bin/env python3
"""Pattern-first TOB history reconstruction from QCU/QCF residuals.

This finds TOB regime boundaries purely algebraically on the residual
R(t) = QCF(t) - QCU(t) (integer cents), without needing basis vectors to
locate the boundaries:

  * Within a stable TOB regime and stable PHA adjustment, R(y, m) is a pure
    function of the calendar month m (exact, in integer cents).
  * A PHA-only step shifts all calendar months by the same constant.
  * A TOB change alters the monthly pattern in a non-constant way.

Phases:
  A. Atomic segmentation: forward and backward scans find maximal runs where
     R is a function of calendar month; their intersection gives "cores" that
     are guaranteed free of any boundary.
  B. Regime chaining: consecutive cores whose patterns differ by a constant
     on common months are the same TOB regime separated by PHA-only steps.
  C. Identification: each regime's pattern is matched (up to a constant)
     against per-code monthly bias vectors obtained by running TOBMain.
     If no exact match exists for all regimes, escalate through basis
     "variants": alternative station coordinates (MSHR location history) and
     alternative LTIX reference windows (leading/trailing truncation of the
     input record, QC-flag exclusion).  A single variant must explain ALL
     regimes of the station (NOAA ran one program over one input file).
  D. Transition resolution: months between cores are assigned exactly to the
     left or right regime (with PHA offsets allowed to step), locating the
     TOB boundary month; a mid-month change is resolved to the exact day by
     searching TOBMain day-weighted blends for an exact match.
  E. Output .his (write_min_his) and strict validation: TOBMain(reconstructed
     .his) vs QCF must leave an exactly piecewise-constant residual.  The
     delivered coordinate is always a documented location (published inventory
     or an MSHR position) -- never a grid-fitted coordinate (see
     defensible_coord).

Usage:
  python3 src/python/qcuf_pattern_to_his.py \
      --inv data/input/station.inv \
      --qcu-dir data/input/raw/tavg --qcf-dir data/output/qcf/tavg \
      --out-history-dir /tmp/his_out --mshr-zip data/mshr_enhanced.txt.zip \
      --tob-bin bin/TOBMain [--test-stations ID1,ID2] [--verbose]
"""

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
import time
from collections import Counter
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

sys.path.insert(0, str(Path(__file__).parent))
import ghcnm_tob_io as qh  # noqa: E402

# ------------------------------------------------------------------------
# Tunables
# ------------------------------------------------------------------------
SOLID_CORE_MIN_OBS = 4     # cores smaller than this are resolved as window months
CHAIN_MIN_COMMON = 2       # min common calendar months to accept a PHA link
LONG_REGIME_MIN_OBS = 24   # regimes this long participate in variant scoring
MAX_COORD_CANDIDATES = 10  # cap on MSHR coordinate variants
MAX_WINDOW_STEPS = 2       # max PHA offset steps inside one transition window
BLEND_ERR_TOLERANCE = 2    # max cents error accepted for a mid-month blend day

# Identification candidates: TRID's single-code basis duplicates 00SS
# (pre-history default is sunset), so it is excluded from matching.
MATCH_CODES = [c for c in qh.ALL_TOB_CODES if c != "TRID"]


def log(msg):
    qh.log(msg)


# ------------------------------------------------------------------------
# Data structures
# ------------------------------------------------------------------------
@dataclass
class Core:
    """A boundary-free interval of observations (indices into times list)."""
    i0: int
    i1: int
    offset: int = 0            # PHA offset relative to regime reference


@dataclass
class Regime:
    """A chained set of cores sharing one TOB code."""
    cores: List[Core]
    pattern: Dict[int, int]    # calendar month -> R cents at reference offset
    code: Optional[str] = None
    basis: Optional[Dict[int, int]] = None   # month -> basis cents
    k: Optional[int] = None    # pattern = basis + k  (k = PHA offset at ref)
    exact: bool = False
    jitter: bool = False       # matched within +/-1 cent (re-derived data era)
    spread: int = 0            # best-match spread when not exact
    start_am: Optional[int] = None   # resolved begin (absolute month, qh units)
    start_day: Optional[int] = None  # resolved begin day (None = day 1)


def dms_quantize(lat: float, lon: float) -> Tuple[float, float]:
    """Round coordinates to the DMS integer-second precision a .his row can
    store, so that identification proves exactness at a representable
    coordinate and the artifact pipeline (tob.use-his-lat-lon=true)
    reproduces it bit-for-bit."""
    lat_d, lat_m, lat_s = qh.decimal_to_dms(lat)
    lon_d, lon_m, lon_s = qh.decimal_to_dms(lon)
    qlat = lat_d + lat_m / 60.0 + lat_s / 3600.0
    # .his stores western longitude as negative degrees with positive min/sec
    qlon = lon_d - lon_m / 60.0 - lon_s / 3600.0
    return qlat, qlon


@dataclass
class Variant:
    """A basis-generation hypothesis: coordinates + input window."""
    label: str
    lat: float
    lon: float
    start_year: Optional[int] = None
    end_year: Optional[int] = None
    drop_qc: bool = False

    def __post_init__(self):
        self.lat, self.lon = dms_quantize(self.lat, self.lon)


@dataclass
class StationResult:
    station_id: str
    status: str                 # PERFECT / IMPERFECT / NO_TOB / SKIP / ERROR
    variant: str = ""
    n_regimes: int = 0
    n_exact: int = 0
    detail: str = ""
    unexplained: int = 0


# ------------------------------------------------------------------------
# Phase A: atomic segmentation
# ------------------------------------------------------------------------
def atomic_forward(times, R):
    segs = []
    start = 0
    monthval = {}
    for i, t in enumerate(times):
        m, v = t[1], R[t]
        if m in monthval and monthval[m] != v:
            segs.append((start, i - 1))
            start = i
            monthval = {}
        monthval[m] = v
    segs.append((start, len(times) - 1))
    return segs


def atomic_backward(times, R):
    n = len(times)
    segs = []
    end = n - 1
    monthval = {}
    for i in range(n - 1, -1, -1):
        t = times[i]
        m, v = t[1], R[t]
        if m in monthval and monthval[m] != v:
            segs.append((i + 1, end))
            end = i
            monthval = {}
        monthval[m] = v
    segs.append((0, end))
    segs.reverse()
    return segs


def consensus_cores(times, R):
    """Intersect forward/backward atomic partitions: pieces free of boundaries."""
    fw = atomic_forward(times, R)
    bw = atomic_backward(times, R)
    cores = []
    for (a0, a1) in fw:
        for (b0, b1) in bw:
            lo, hi = max(a0, b0), min(a1, b1)
            if lo <= hi:
                cores.append((lo, hi))
    cores.sort()
    return cores


def pattern_of(times, R, i0, i1):
    p = {}
    for i in range(i0, i1 + 1):
        t = times[i]
        p[t[1]] = R[t]
    return p


# ------------------------------------------------------------------------
# Phase B: regime chaining
# ------------------------------------------------------------------------
def chain_regimes(times, R, cores) -> List[Regime]:
    """Chain solid cores whose patterns differ by a constant into regimes.

    Small cores (< SOLID_CORE_MIN_OBS) are left out; their months are
    resolved during transition resolution.
    """
    solid = [(i0, i1) for (i0, i1) in cores if i1 - i0 + 1 >= SOLID_CORE_MIN_OBS]
    if not solid:
        # degenerate: fall back to the largest core, if any
        if cores:
            solid = [max(cores, key=lambda c: c[1] - c[0])]
        else:
            return []

    regimes: List[Regime] = []
    cur: Optional[Regime] = None
    for (i0, i1) in solid:
        pat = pattern_of(times, R, i0, i1)
        if cur is not None:
            common = set(cur.pattern) & set(pat)
            if len(common) >= CHAIN_MIN_COMMON:
                diffs = {pat[m] - cur.pattern[m] for m in common}
                if len(diffs) == 1:
                    off = diffs.pop()
                    for m, v in pat.items():
                        cur.pattern.setdefault(m, v - off)
                    cur.cores.append(Core(i0, i1, off))
                    continue
            regimes.append(cur)
        cur = Regime(cores=[Core(i0, i1, 0)], pattern=dict(pat))
    if cur is not None:
        regimes.append(cur)
    return regimes


# ------------------------------------------------------------------------
# Phase C: basis generation + identification
# ------------------------------------------------------------------------
def transform_raw(src: Path, dst: Path, start_year=None, end_year=None,
                  drop_qc=False):
    """Write a transformed copy of a raw data file (LTIX window variants)."""
    with open(src) as f, open(dst, "w") as g:
        for line in f:
            if len(line) < 16:
                continue
            try:
                year = int(line[12:16])
            except ValueError:
                continue
            if start_year and year < start_year:
                continue
            if end_year and year > end_year:
                continue
            if drop_qc:
                buf = list(line.rstrip("\n"))
                for m in range(12):
                    s = 16 + m * 9
                    if len(line) > s + 7 and line[s + 7] != " ":
                        buf[s:s + 6] = list(f"{-9999:6d}")
                line = "".join(buf) + "\n"
            g.write(line)


def run_tob_batch(tob_bin: Path, work_dir: Path,
                  entries: List[Tuple[str, float, float, Path, str]],
                  timeout=120, use_his_lat_lon=False
                  ) -> Dict[str, Dict[Tuple[int, int], int]]:
    """Run one TOBMain over many synthetic stations.

    entries: list of (fake_id, lat, lon, raw_file, his_content).
    Returns fake_id -> parsed adjusted data.
    """
    raw_dir = work_dir / "raw" / "tavg"
    tob_dir = work_dir / "tob" / "tavg"
    hist_dir = work_dir / "history"
    for d in (raw_dir, tob_dir, hist_dir):
        d.mkdir(parents=True, exist_ok=True)

    inv_file = work_dir / "station.inv"
    with open(inv_file, "w") as f:
        for fake_id, lat, lon, raw_file, his_content in entries:
            f.write(f"{fake_id:11s} {lat:8.4f}  {lon:9.4f}   100.0 \n")
            dst = raw_dir / f"{fake_id}.raw.tavg"
            if not dst.exists():
                try:
                    os.link(raw_file, dst)
                except OSError:
                    shutil.copy(raw_file, dst)
            with open(hist_dir / f"{fake_id}.his", "w") as h:
                h.write(his_content + "\n")

    props = work_dir / "tob.properties"
    with open(props, "w") as f:
        f.write(f"""pha.element = tavg
pha.path.station-metadata = {inv_file}
pha.path.station-history = {hist_dir}/
tob.start-year = 1700
tob.start-from-history = true
tob.use-his-lat-lon = {"true" if use_his_lat_lon else "false"}
tob.input-data-type = raw
tob.backfill-if-first-nonblank = false
tob.pause-on-blank-after-nonblank = false
tob.path.station-element-data-in = {raw_dir}/
tob.path.station-element-data-out = {tob_dir}/
tob.logger.filename = {work_dir}/tob.log
tob.logger.level = ERROR
tob.logger.print-to-stdout = false
tob.logger.append-datestamp = false
tob.logger.rollover-datestamp = false
""")
    try:
        res = subprocess.run([str(tob_bin), "-p", str(props)], cwd=work_dir,
                             capture_output=True, text=True, timeout=timeout)
        if res.returncode != 0:
            return {}
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return {}

    out = {}
    for fake_id, *_ in entries:
        p = tob_dir / f"{fake_id}.tob.tavg"
        if p.exists():
            out[fake_id] = qh.parse_station_data(p)
    return out


def single_code_his_line(code: str, lat: float, lon: float,
                         span: Tuple[Tuple[int, int], Tuple[int, int]]) -> str:
    lat_d, lat_m, lat_s = qh.decimal_to_dms(lat)
    lon_d, lon_m, lon_s = qh.decimal_to_dms(lon)
    (y0, m0), (y1, m1) = span
    return (
        f"2{' ' * 12}"
        f" {y0:4d}{m0:02d}01"
        f" {y1:4d}{m1:02d}28"
        f" {lat_d:3.0f}{lat_m:3.0f}{lat_s:3.0f}"
        f" {lon_d:4.0f}{lon_m:3.0f}{lon_s:3.0f}"
        f" {' ' * 11}"
        f" {100:5d}"
        f"  {' ' * 4}"
        f" {code:4s}"
        f"     {' ' * 66}"
    )


def gen_bases(sid, variant: Variant, qcu_file: Path, tob_bin: Path
              ) -> Dict[str, Dict[int, int]]:
    """Per-code monthly bias vectors (cents) under a variant. One TOBMain run."""
    with tempfile.TemporaryDirectory() as td:
        work_dir = Path(td)
        raw = work_dir / "in.raw.tavg"
        transform_raw(qcu_file, raw, variant.start_year, variant.end_year,
                      variant.drop_qc)
        qcu_data = qh.parse_station_data(raw)
        if not qcu_data:
            return {}
        ts = sorted(qcu_data)
        span = (ts[0], ts[-1])
        entries = []
        idx = {}
        for i, code in enumerate(qh.ALL_TOB_CODES):
            fake_id = f"{sid[:9]}{i:02d}"
            idx[fake_id] = code
            his = single_code_his_line(code, variant.lat, variant.lon, span)
            entries.append((fake_id, variant.lat, variant.lon, raw, his))
        results = run_tob_batch(tob_bin, work_dir, entries)
        bases = {}
        for fake_id, code in idx.items():
            if fake_id not in results:
                continue
            data = results[fake_id]
            bym: Dict[int, Counter] = {}
            for (y, m), v in data.items():
                if (y, m) in qcu_data:
                    bym.setdefault(m, Counter())[v - qcu_data[(y, m)]] += 1
            if not bym:
                continue
            bases[code] = {m: c.most_common(1)[0][0] for m, c in bym.items()}
        return bases


def code_preference(code: str) -> Tuple:
    """Deterministic ordering for tie-breaks: common codes, then hour, special."""
    if code in qh.COMMON_TOB_CODES:
        return (0, qh.COMMON_TOB_CODES.index(code))
    if code.endswith("HR"):
        return (1, code)
    return (2, code)


def match_pattern(pattern: Dict[int, int], bases: Dict[str, Dict[int, int]],
                  codes=None):
    """Return (exact_list, jitter_list, best).

    exact_list  = [(code, k)] with pattern == basis + k, by preference.
    jitter_list = [(code, k)] with spread exactly 1 (+/-1-cent data
                  re-derivation jitter); k is the majority offset.
    best        = (code, spread, k_mid) for the closest miss."""
    exact = []
    jitter = []
    best = None
    for code in (codes or MATCH_CODES):
        vec = bases.get(code)
        if not vec:
            continue
        diffs = [pattern[m] - vec[m] for m in pattern if m in vec]
        if len(diffs) != len(pattern):
            continue
        spread = max(diffs) - min(diffs)
        if spread == 0:
            exact.append((code, diffs[0]))
        elif spread == 1:
            jitter.append((code, Counter(diffs).most_common(1)[0][0]))
        if best is None or spread < best[1]:
            best = (code, spread, round(sum(diffs) / len(diffs)))
    exact.sort(key=lambda ck: code_preference(ck[0]))
    jitter.sort(key=lambda ck: code_preference(ck[0]))
    return exact, jitter, best



def choose_code(exact, jit, prev_code, nobs):
    """Pick (code, k, is_exact) from match lists.

    Prefers exact matches; but for short regimes whose exact matches are all
    unusual codes, a common or neighbour code matching within +/-1-cent
    jitter is more plausible than an exotic exact fit to re-derived data.
    Returns None if both lists are empty."""
    if not exact and not jit:
        return None
    ex_codes = [c for c, k in exact]
    if exact:
        if prev_code in ex_codes:
            return prev_code, dict(exact)[prev_code], True
        common_ex = [c for c in ex_codes if c in qh.COMMON_TOB_CODES]
        if common_ex or nobs >= LONG_REGIME_MIN_OBS or not jit:
            code = common_ex[0] if common_ex else ex_codes[0]
            return code, dict(exact)[code], True
        # short regime, only unusual exact codes: prefer plausible jitter
        jt_codes = [c for c, k in jit]
        if prev_code in jt_codes:
            return prev_code, dict(jit)[prev_code], False
        common_jt = [c for c in jt_codes if c in qh.COMMON_TOB_CODES]
        if common_jt:
            return common_jt[0], dict(jit)[common_jt[0]], False
        return ex_codes[0], dict(exact)[ex_codes[0]], True
    jt_codes = [c for c, k in jit]
    code = prev_code if prev_code in jt_codes else jt_codes[0]
    return code, dict(jit)[code], False


def build_variants(inv_entry, mshr_records, regimes, times) -> List[Variant]:
    """Ordered list of basis variants to try (cheapest / most likely first)."""
    inv_ll = (inv_entry.lat, inv_entry.lon)
    coords: List[Tuple[str, float, float]] = [("inv", *inv_ll)]
    seen = {(round(inv_ll[0], 4), round(inv_ll[1], 4))}
    # most recent MSHR coordinates first
    for rec in reversed(mshr_records):
        if rec.lat is None or rec.lon is None:
            continue
        key = (round(rec.lat, 4), round(rec.lon, 4))
        if key in seen:
            continue
        seen.add(key)
        coords.append((f"mshr{rec.begin_date.year}", rec.lat, rec.lon))
        if len(coords) >= MAX_COORD_CANDIDATES:
            break

    variants = []
    variants.append(Variant("full", *inv_ll))
    for name, lat, lon in coords[1:]:
        variants.append(Variant(name, lat, lon))

    # coordinates are expressible in the artifact (.his rows); LTIX windows
    # are not, so start-year variants are a last resort (tier 3)
    for name, lat, lon in coords[:2]:
        variants.append(Variant(f"{name}+qc", lat, lon, drop_qc=True))
    return variants


def identify_regimes(sid, regimes: List[Regime], inv_entry, mshr_records,
                     times, qcu_file, tob_bin, verbose=False
                     ) -> Tuple[Variant, Dict[str, Dict[int, int]]]:
    """Choose the basis variant that makes the most (ideally all) long regimes
    match exactly; assign codes to every regime.  Returns (variant, bases).
    """
    longs = [rg for rg in regimes
             if sum(c.i1 - c.i0 + 1 for c in rg.cores) >= LONG_REGIME_MIN_OBS]
    scored = longs if longs else regimes

    variants = build_variants(inv_entry, mshr_records, regimes, times)
    best_choice = None   # (n_miss, total_spread, variant, bases)

    def try_variants(vlist):
        nonlocal best_choice
        for variant in vlist:
            bases = gen_bases(sid, variant, qcu_file, tob_bin)
            if not bases:
                continue
            n_miss = 0
            n_jitter = 0
            total_spread = 0
            for rg in scored:
                exact, jit, best = match_pattern(rg.pattern, bases)
                if exact:
                    continue
                if jit:
                    n_jitter += 1
                    continue
                n_miss += 1
                total_spread += best[1] if best else 9999
            if verbose:
                log(f"  variant {variant.label}: miss={n_miss} "
                    f"jitter={n_jitter} spread={total_spread}")
            key = (n_miss, n_jitter, total_spread)
            if best_choice is None or key < best_choice[:3]:
                best_choice = (n_miss, n_jitter, total_spread, variant, bases)
            if n_miss == 0 and n_jitter == 0:
                return True
        return False

    if not try_variants(variants):
        # tier 2: arc-minute coordinate grid -- used ONLY to IDENTIFY the obs
        # codes for the handful of stations whose documented coordinate leaves
        # the sub-cent identifier just short.  A whole contour of coordinates
        # yields the same integer bias table, so the grid point is a computational
        # aid, not a delivered location: defensible_coord ALWAYS rewrites the
        # final .his to a documented coordinate (inventory or MSHR position) that
        # reproduces the same output, or keeps inventory + a residual.  No fitted
        # coordinate is ever delivered.
        base_lat, base_lon = inv_entry.lat, inv_entry.lon
        coarse = [Variant(f"grid{dlat:+d}{dlon:+d}",
                          base_lat + dlat / 60, base_lon + dlon / 60)
                  for dlat in range(-6, 7, 2)
                  for dlon in range(-12, 13, 2)]
        done = try_variants(coarse)
        if not done and best_choice is not None:
            bl, bo = best_choice[3].lat, best_choice[3].lon
            fine = [Variant(f"fine{i:+d}{j:+d}", bl + i / 60, bo + j / 60)
                    for i in (-1, 0, 1) for j in (-1, 0, 1)
                    if (i, j) != (0, 0)]
            done = try_variants(fine)
        if not done:
            # tier 3 (last resort, code-identification only, not a location):
            # LTIX input-window variants -- the record span NOAA had that today's
            # QCU lacks.
            tried = {v.label for v in variants}
            coords = [(v.label, v.lat, v.lon) for v in variants
                      if v.start_year is None and not v.drop_qc][:3]
            y0 = times[0][0]
            y1 = times[-1][0]
            deep: List[Variant] = []
            for name, lat, lon in coords:
                for sy in range(y0 + 1, min(y0 + 61, y1), 2):
                    lbl = f"{name}+start{sy}"
                    if lbl not in tried:
                        deep.append(Variant(lbl, lat, lon, start_year=sy))
            for name, lat, lon in coords[:2]:
                for ey in range(1980, min(2026, y1 + 1), 5):
                    deep.append(Variant(f"{name}+end{ey}", lat, lon, end_year=ey))
            try_variants(deep)

    _, _, _, variant, bases = best_choice
    prev_code = None
    for rg in regimes:
        exact, jit, best = match_pattern(rg.pattern, bases)
        nobs = sum(c.i1 - c.i0 + 1 for c in rg.cores)
        picked = choose_code(exact, jit, prev_code, nobs)
        if picked:
            code, k, is_exact = picked
            rg.code, rg.k = code, k
            rg.exact = is_exact
            rg.jitter = not is_exact
            rg.basis = bases[code]
        else:
            rg.code, rg.spread = best[0], best[1]
            rg.k = best[2]
            rg.exact = False
            rg.basis = bases[best[0]]
        prev_code = rg.code
    return variant, bases


# ------------------------------------------------------------------------
# Phase D: transition resolution
# ------------------------------------------------------------------------
FLAT_BASIS = {m: 0 for m in range(1, 13)}


def virtual_flat_regime() -> Regime:
    """A no-adjustment (24HR) pseudo-regime used before the first / after the
    last real regime."""
    return Regime(cores=[], pattern={}, code=qh.TOB_NO_BIAS_CODE,
                  basis=dict(FLAT_BASIS), k=None, exact=True)


def offsets_needed(times, R, idxs, basis):
    """For observation indices, the PHA offset each month would need under
    the given basis. Returns list of (idx, offset) or None if basis lacks
    a month."""
    out = []
    for i in idxs:
        t = times[i]
        b = basis.get(t[1])
        if b is None:
            return None
        out.append((i, R[t] - b))
    return out


def count_steps(seq: List[int]) -> int:
    return sum(1 for a, b in zip(seq, seq[1:]) if a != b)


MIN_STEP_SPACING = 12   # PHA steps must be at least this many months apart


def side_fit(items: List[Tuple[int, int]], anchor_l: Optional[int],
             anchor_r: Optional[int], anchor_r_am: Optional[int],
             max_steps: int) -> Optional[Tuple[int, int]]:
    """Fit an offset sequence as piecewise-constant with limited, spaced steps.

    items: [(abs_month, offset)] for window months on one side of a TOB
    boundary; anchor_l/anchor_r are the known offsets of the adjacent cores
    (None = unconstrained); anchor_r_am is the absolute month at which the
    right core starts.  Months may be deleted (data-revision deviants).

    A level change is only allowed when the current level has persisted for
    >= MIN_STEP_SPACING months, or when a data gap of >= MIN_STEP_SPACING
    months hides the true step timing.

    Returns (min_deletions, steps_used) for the best fit with steps <=
    max_steps."""
    FAR = -10 ** 9
    # state: (level, steps, level_start_am, last_kept_am) -> min deletions
    states = {(anchor_l, 0, FAR, FAR): 0}
    for am, v in items:
        nxt: Dict[Tuple[Optional[int], int, int, int], int] = {}

        def upd(key, cost):
            if key not in nxt or nxt[key] > cost:
                nxt[key] = cost

        for (level, steps, start_am, last_am), dels in states.items():
            # delete this month
            upd((level, steps, start_am, last_am), dels + 1)
            # keep it
            if level is None:
                upd((v, steps, am, am), dels)
            elif v == level:
                upd((level, steps, start_am, am), dels)
            elif steps < max_steps and (am - start_am >= MIN_STEP_SPACING or
                                        am - last_am >= MIN_STEP_SPACING):
                upd((v, steps + 1, am, am), dels)
        states = nxt
    best = None
    for (level, steps, start_am, last_am), dels in states.items():
        if anchor_r is not None and level is not None and level != anchor_r:
            if steps >= max_steps:
                continue
            am = anchor_r_am if anchor_r_am is not None else last_am + 1
            if not (am - start_am >= MIN_STEP_SPACING or
                    am - last_am >= MIN_STEP_SPACING):
                continue
            final_steps = steps + 1
        else:
            final_steps = steps
        cand = (dels, final_steps)
        if best is None or cand < best:
            best = cand
    return best


def resolve_window(times, R, left: Optional[Regime], right: Optional[Regime],
                   w_lo: int, w_hi: int, max_steps=MAX_WINDOW_STEPS):
    """Assign window observations (indices w_lo..w_hi inclusive) to the left
    or right regime, locating the TOB switch.

    left/right may be virtual regimes (no cores) whose boundary offset is
    unconstrained.

    Returns (q_idx, blend_idx, n_unexplained):
      q_idx     = index of first observation under the right regime's code
                  (may be w_hi+1 => all window months belong to left)
      blend_idx = index of a transition month needing a day-level blend, or None
    """
    idxs = list(range(w_lo, w_hi + 1))
    if not idxs:
        return w_hi + 1, None, 0, None

    left_anchor = ([left.cores[-1].offset + (left.k or 0)]
                   if left is not None and left.cores else [])
    right_anchor = ([right.cores[0].offset + (right.k or 0)]
                    if right is not None and right.cores else [])
    lbasis = left.basis if left is not None else None
    rbasis = right.basis if right is not None else None

    al = left_anchor[0] if left_anchor else None
    ar = right_anchor[0] if right_anchor else None

    best = None  # (cost tuple, qpos, blend)
    for qpos in range(len(idxs) + 1):
        if lbasis is None and qpos > 0:
            continue
        if rbasis is None and qpos < len(idxs):
            continue
        for blend_first_right in (False, True):
            lneed = offsets_needed(times, R, idxs[:qpos], lbasis or {})
            rneed = offsets_needed(times, R, idxs[qpos:], rbasis or {})
            if lneed is None or rneed is None:
                continue
            blend = None
            litems = [(qh.to_absolute_month(*times[i]), o) for i, o in lneed]
            ritems = [(qh.to_absolute_month(*times[i]), o) for i, o in rneed]
            if blend_first_right:
                if not rneed or lbasis is None or rbasis is None:
                    continue
                # transition month: value must sit strictly between the
                # pure-left and pure-right predictions under the PHA offset
                # holding AT that month (day-weighted average of biases).
                # Candidate offsets from nearest evidence outward: the next
                # right-side month's level, the far right anchor (may lie
                # beyond an in-gap PHA step), the left-side level, the left
                # anchor.
                bi, _ = rneed[0]
                t = times[bi]
                lb, rb = lbasis.get(t[1]), rbasis.get(t[1])
                if lb is None or rb is None:
                    continue
                cand_offs = []
                if len(ritems) > 1:
                    cand_offs.append(ritems[1][1])
                if ar is not None:
                    cand_offs.append(ar)
                if litems:
                    cand_offs.append(litems[-1][1])
                if al is not None:
                    cand_offs.append(al)
                blend_off = None
                for ro in cand_offs:
                    if min(lb, rb) < R[t] - ro < max(lb, rb):
                        blend_off = ro
                        break
                if blend_off is None:
                    continue
                blend = bi
                ritems = ritems[1:]
            r_core_am = None
            if right is not None and right.cores:
                r_core_am = qh.to_absolute_month(*times[right.cores[0].i0])
            # One PHA offset function across the whole window: TOB boundary
            # does not reset the offset level (PHA is a separate program).
            fit = side_fit(litems + ritems, al, ar, r_core_am, max_steps)
            if fit is None:
                continue
            misfits, steps = fit
            has_blend = 1 if blend is not None else 0
            cost = (misfits, steps, has_blend, -qpos)
            if best is None or cost < best[0]:
                best = (cost, qpos, blend, blend_off if blend is not None else None)
    if best is None:
        return idxs[0], None, len(idxs), None
    _, qpos, blend, blend_off = best
    unexplained = best[0][0]
    q_idx = idxs[qpos] if qpos < len(idxs) else w_hi + 1
    return q_idx, blend, unexplained, blend_off


def resolve_window_dp(times, R, left: Optional[Regime], right: Optional[Regime],
                      w_lo: int, w_hi: int,
                      bases: Dict[str, Dict[int, int]]):
    """General window resolver: any sequence of TOB codes across the window,
    with day-weighted blend months at switches, deletions (data revisions)
    and at most one PHA step (offset o1 -> o2).

    A code must persist for >= 3 kept observations before switching away
    (except the immediate switch at the window start).  Returns
    (events, unexplained) with events = [(idx, code, blend, offset)] ending
    in the right regime's code, or None.
    """
    if w_hi < w_lo:
        return None
    lcode = left.code if left is not None else qh.TOB_NO_BIAS_CODE
    rcode = right.code if right is not None else None
    al = (left.cores[-1].offset + (left.k or 0)) \
        if (left is not None and left.cores) else None
    ar = (right.cores[0].offset + (right.k or 0)) \
        if (right is not None and right.cores) else None
    r_core_i0 = right.cores[0].i0 if (right is not None and right.cores) else None

    # Split the window at data gaps >= MIN_STEP_SPACING months: a PHA step
    # (or several) can hide inside a gap, so each piece gets its own offset
    # freedom while the TOB code is stitched across.
    subs = []
    s = w_lo
    for i in range(w_lo, w_hi):
        am1 = qh.to_absolute_month(*times[i])
        am2 = qh.to_absolute_month(*times[i + 1])
        if am2 - am1 >= MIN_STEP_SPACING:
            subs.append((s, i))
            s = i + 1
    subs.append((s, w_hi))

    # gap between the window end and the right core start frees that anchor
    tail_gap = False
    if r_core_i0 is not None:
        am1 = qh.to_absolute_month(*times[w_hi])
        am2 = qh.to_absolute_month(*times[r_core_i0])
        tail_gap = am2 - am1 >= MIN_STEP_SPACING

    # gap between the left core's last obs and the window's first obs frees
    # the left offset anchor too (a PHA step can hide there)
    if al is not None and w_lo > 0:
        am1 = qh.to_absolute_month(*times[w_lo - 1])
        am2 = qh.to_absolute_month(*times[w_lo])
        if am2 - am1 >= MIN_STEP_SPACING:
            al = None

    events = []
    dels = 0
    jits = 0
    cur_code, cur_off = lcode, al
    for si, (a, b) in enumerate(subs):
        is_last = si == len(subs) - 1
        rc = rcode if is_last else None
        ar_eff = ar if (is_last and not tail_gap) else None
        res = _window_dp(times, R, cur_code, cur_off, rc, ar_eff,
                         r_core_i0 if is_last else None, a, b, bases)
        if res is None:
            dels += b - a + 1
            cur_off = None
            continue
        ev, d, j, final_code = res
        events.extend(ev)
        dels += d
        jits += j
        cur_code, cur_off = final_code, None
    if rcode is not None and cur_code != rcode:
        eidx = r_core_i0 if r_core_i0 is not None else w_hi
        events.append((eidx, rcode, False, ar if ar is not None else 0))
    return events, dels, jits


def _window_dp(times, R, lcode, al, rcode, ar, r_core_i0,
               w_lo, w_hi, bases):
    """Core window DP with explicit anchors.

    lcode/al: code and PHA offset entering the window (al None = offset
    unconstrained, e.g. after a long data gap).  rcode/ar: required code /
    offset at the window end (None = unconstrained).  Returns
    (events, deletions, jitters, final_code) or None."""
    idxs = list(range(w_lo, w_hi + 1))
    n = len(idxs)
    if n == 0 or n > 150:
        return None

    codes = {c: v for c, v in bases.items() if c != "TRID"}
    if lcode not in codes:
        codes[lcode] = FLAT_BASIS if lcode == qh.TOB_NO_BIAS_CODE else None
        if codes[lcode] is None:
            return None

    if al is not None and ar is not None:
        pairs = [(al, ar)]
    elif al is not None:
        pairs = [(al, al)]
    else:
        # offset unconstrained entering the window: seed candidates from the
        # first few observations under every code (plus ar when known) —
        # the very first obs may itself be a blend or deviant month
        cands = set()
        if ar is not None:
            cands.add(ar)
        for i in idxs[:3]:
            t0 = times[i]
            for c, vec in codes.items():
                if vec and t0[1] in vec:
                    cands.add(R[t0] - vec[t0[1]])
        if not cands:
            return None
        if ar is not None:
            pairs = [(o1, ar) for o1 in cands]
        else:
            pairs = [(o1, o1) for o1 in cands]

    best_overall = None  # (cost, events, o1, o2)
    for (o1, o2) in pairs:
        # layers[pos][state] = (cost, prev_state, action)
        # state = (code, age 1..3, phase 0/1)
        # cost = (dels, jitters, switches, blends, steps); a jitter is a
        # kept month off by exactly one cent (re-derived data era)
        start = (lcode, 3, 0)
        layer = {start: ((0, 0, 0, 0, 0), None, None)}
        layers = []
        for pos, k in enumerate(idxs):
            t = times[k]
            m, v = t[1], R[t]
            nxt: Dict = {}

            def upd(state, cost, prev, action):
                if state not in nxt or nxt[state][0] > cost:
                    nxt[state] = (cost, prev, action)

            for state, (cost, _p, _a) in layer.items():
                code, age, phase = state
                # optional phase advance (PHA step) before consuming this obs
                variants_ = [(state, cost, None)]
                if o1 != o2 and phase == 0 and cost[4] == 0:
                    variants_.append((
                        (code, age, 1),
                        (cost[0], cost[1], cost[2], cost[3], 1), "step"))
                for (st, cst, stepact) in variants_:
                    c2, a2, ph2 = st
                    off = o1 if ph2 == 0 else o2
                    vec = codes.get(c2)
                    b_cur = vec.get(m) if vec else None
                    # delete
                    upd((c2, a2, ph2),
                        (cst[0] + 1, cst[1], cst[2], cst[3], cst[4]),
                        state, (stepact, "del", None))
                    # keep (exact or one-cent jitter)
                    if b_cur is not None:
                        d = v - b_cur - off
                        if d == 0:
                            upd((c2, min(a2 + 1, 3), ph2), cst,
                                state, (stepact, "keep", None))
                        elif abs(d) == 1:
                            upd((c2, min(a2 + 1, 3), ph2),
                                (cst[0], cst[1] + 1, cst[2], cst[3], cst[4]),
                                state, (stepact, "keep", None))
                    # switch to another code
                    if a2 >= 3 or pos == 0:
                        for c3, vec3 in codes.items():
                            if c3 == c2 or vec3 is None or m not in vec3:
                                continue
                            b_new = vec3[m]
                            d = v - b_new - off
                            if d == 0:
                                upd((c3, 1, ph2),
                                    (cst[0], cst[1], cst[2] + 1, cst[3],
                                     cst[4]),
                                    state, (stepact, "switch", c3))
                            elif abs(d) == 1:
                                upd((c3, 1, ph2),
                                    (cst[0], cst[1] + 1, cst[2] + 1, cst[3],
                                     cst[4]),
                                    state, (stepact, "switch", c3))
                            elif (b_cur is not None and
                                  min(b_cur, b_new) < v - off < max(b_cur, b_new)):
                                upd((c3, 1, ph2),
                                    (cst[0], cst[1], cst[2] + 1, cst[3] + 1,
                                     cst[4]),
                                    state, (stepact, "blend", c3))
            layers.append(nxt)
            layer = nxt
            if not layer:
                break
        if not layer:
            continue

        # pick final state: must end in rcode (or pay one extra switch at the
        # right core start).  When o1 != o2 and the phase never advanced, the
        # PHA step is taken to occur between the window end and the right
        # core (counted as a step).
        for state, (cost, _p, _a) in layer.items():
            code, age, phase = state
            late_step = 1 if (o1 != o2 and phase == 0) else 0
            extra = 0
            if rcode is not None and code != rcode:
                if age < 3:
                    continue
                extra = 1
            total = (cost[0], cost[1], cost[2] + extra, cost[3],
                     cost[4] + late_step)
            if best_overall is not None and total >= best_overall[0]:
                continue
            # reconstruct events
            events = []
            st = state
            for pos in range(n - 1, -1, -1):
                cst, prev, action = layers[pos][st]
                stepact, kind, newcode = action
                off = o1 if st[2] == 0 else o2
                if kind in ("switch", "blend"):
                    events.append((idxs[pos], newcode, kind == "blend", off))
                st = prev
                if stepact == "step":
                    pass  # PHA step position not needed for .his output
            events.reverse()
            if extra:
                eidx = r_core_i0 if r_core_i0 is not None else w_hi
                events.append((eidx, rcode, False,
                               o2 if o1 != o2 else o1))
            best_overall = (total, events, o1, o2, code if not extra else rcode)

    if best_overall is None:
        return None
    total, events, o1, o2, final_code = best_overall
    return events, total[0], total[1], final_code


def resolve_blend_day(sid, times, R, left_code: str, right_code: str,
                      blend_idx: int, ref_off: int,
                      variant: Variant, qcu_file: Path, tob_bin: Path
                      ) -> Optional[int]:
    """Find the exact day d such that a .his switching codes at (q, d)
    reproduces the observed blend month value. One TOBMain batch."""
    t = times[blend_idx]
    y, m = t
    ndays = [31, 29 if (y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)) else 28,
             31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1]
    with tempfile.TemporaryDirectory() as td:
        work_dir = Path(td)
        raw = work_dir / "in.raw.tavg"
        transform_raw(qcu_file, raw, variant.start_year, variant.end_year,
                      variant.drop_qc)
        qcu_data = qh.parse_station_data(raw)
        ts = sorted(qcu_data)
        span0, span1 = ts[0], ts[-1]
        lat_d, lat_m, lat_s = qh.decimal_to_dms(variant.lat)
        lon_d, lon_m, lon_s = qh.decimal_to_dms(variant.lon)

        def row(code, b, e, bday=1, eday=28):
            return (f"2{' ' * 12}"
                    f" {b[0]:4d}{b[1]:02d}{bday:02d}"
                    f" {e[0]:4d}{e[1]:02d}{eday:02d}"
                    f" {lat_d:3.0f}{lat_m:3.0f}{lat_s:3.0f}"
                    f" {lon_d:4.0f}{lon_m:3.0f}{lon_s:3.0f}"
                    f" {' ' * 11}"
                    f" {100:5d}"
                    f"  {' ' * 4}"
                    f" {code:4s}"
                    f"     {' ' * 66}")

        entries = []
        idx = {}
        for d in range(2, ndays + 1):
            fake_id = f"{sid[:9]}{d:02d}"
            idx[fake_id] = d
            his = (row(left_code, span0, (y, m)) + "\n" +
                   row(right_code, (y, m), span1, bday=d))
            entries.append((fake_id, variant.lat, variant.lon, raw, his))
        results = run_tob_batch(tob_bin, work_dir, entries)
        best = None  # (err, day)
        for fake_id, d in sorted(idx.items(), key=lambda kv: kv[1]):
            data = results.get(fake_id)
            if not data or t not in data or t not in qcu_data:
                continue
            b_blend = data[t] - qcu_data[t]
            err = abs(b_blend + ref_off - R[t])
            if err == 0:
                return d, 0
            if best is None or err < best[0]:
                best = (err, d)
        # TOBMain's day-weighting quantizes the blend to a coarse cent grid;
        # a QCU revision of the transition month can leave the needed value
        # between two achievable days.  A minimal-error day is then far
        # closer to the truth than pushing the boundary to the month edge.
        if best is not None and best[0] <= BLEND_ERR_TOLERANCE:
            return best[1], best[0]
    return None


# ------------------------------------------------------------------------
# Phase E: assembly + validation
# ------------------------------------------------------------------------
def assemble_segments(seg_specs: List[Tuple[int, Optional[int], str]],
                      end_am: int) -> List[qh.TobSegment]:
    """Build TobSegments from ordered (start_am, start_day, code) rows.

    Each segment's end is the month before the next segment's start (or the
    last observed month for the final segment); write_his_file derives exact
    end dates from the following row's begin date."""
    # merge consecutive same-code rows; drop rows made degenerate by an
    # immediately-following row starting at the same month
    rows: List[Tuple[int, Optional[int], str]] = []
    for spec in seg_specs:
        if rows and rows[-1][2] == spec[2] and spec[1] is None:
            continue
        if rows and spec[1] is None and rows[-1][0] >= spec[0]:
            rows.pop()
            if rows and rows[-1][2] == spec[2]:
                continue
        rows.append(spec)
    segs = []
    for i, (start_am, start_day, code) in enumerate(rows):
        if i + 1 < len(rows):
            nxt = rows[i + 1][0]
            seg_end = max(start_am, nxt - 1 if rows[i + 1][1] is None else nxt)
        else:
            seg_end = max(start_am, end_am)
        segs.append(qh.TobSegment(start_am, seg_end, code, 0.0,
                                  include_in_his=True, start_day=start_day))
    return segs


def his_rows_for(segments, lat, lon) -> List[str]:
    """Render segments as minimal .his rows carrying the given coordinates.

    One row per TOB regime; begin dates keep day-level boundaries; each end
    date is the day before the next row's begin; the final row runs to
    99991231 so TOBMain's timeline covers all data."""
    import datetime as dt
    lat_d, lat_m, lat_s = qh.decimal_to_dms(lat)
    lon_d, lon_m, lon_s = qh.decimal_to_dms(lon)
    rows = []
    for i, seg in enumerate(segments):
        y0, m0 = qh.from_absolute_month(seg.start_month)
        d0 = seg.start_day or 1
        if i + 1 < len(segments):
            ny, nm = qh.from_absolute_month(segments[i + 1].start_month)
            nd = segments[i + 1].start_day or 1
            end = dt.date(ny, nm, nd) - dt.timedelta(days=1)
            end_str = f"{end.year:4d}{end.month:02d}{end.day:02d}"
        else:
            end_str = "99991231"
        rows.append(
            f"2{' ' * 12}"
            f" {y0:4d}{m0:02d}{d0:02d}"
            f" {end_str}"
            f" {lat_d:3d}{lat_m:3d}{lat_s:3d}"
            f" {lon_d:4d}{lon_m:3d}{lon_s:3d}"
            f" {' ' * 11}"
            f" {100:5d}"
            f"  {' ' * 4}"
            f" {seg.tob_code:4s}"
            f"     {' ' * 66}")
    return rows


def write_min_his(sid, segments, lat, lon, out_dir: Path):
    with open(out_dir / f"{sid}.his", "w") as f:
        for row in his_rows_for(segments, lat, lon):
            f.write(row + "\n")


def strict_validate(sid, segments, lat, lon, qcu_file, qcf_data, tob_bin,
                    verbose=False) -> Tuple[bool, int]:
    """Run TOBMain with the reconstructed history over the full QCU record and
    check that QCF - TOB is exactly piecewise constant.

    lat/lon should be the coordinates of the chosen identification variant so
    that the bias tables match the ones used for reconstruction.

    Returns (passes_legacy_filter, n_deviant_months) where deviant months are
    isolated one/two-month runs sandwiched between longer runs.

    The validation .his is rendered by the same writer that produces the
    artifact, and TOBMain runs with tob.use-his-lat-lon=true, so this tests
    exactly what the operational pipeline will reproduce."""
    with tempfile.TemporaryDirectory() as td:
        work_dir = Path(td)
        entries = [(sid, lat, lon, qcu_file,
                    "\n".join(his_rows_for(segments, lat, lon)))]
        results = run_tob_batch(tob_bin, work_dir, entries,
                                use_his_lat_lon=True)
        tob_data = results.get(sid)
        if not tob_data:
            return False, 99999, 0
        r2 = {}
        for t, qv in qcf_data.items():
            if t in tob_data:
                r2[t] = qv - tob_data[t]
        ts = sorted(r2)
        vals = [r2[t] for t in ts]
        # runs of equal values
        runs = []
        for v in vals:
            if runs and runs[-1][0] == v:
                runs[-1][1] += 1
            else:
                runs.append([v, 1])
        # deviants: obs in short runs (<=2) whose value does not match the
        # nearest long run on either side (isolated deviants can split a
        # legitimate run; the fragments then still match the long level).
        # Values within one cent of the nearest long level are counted
        # separately as jitter: the signature of QCU months re-derived
        # (e.g. from daily data) since the NOAA run.
        long_min = 3
        deviants = 0
        jitter = 0
        for i, (v, n) in enumerate(runs):
            if n > 2:
                continue
            near = []
            for j in range(i - 1, -1, -1):
                if runs[j][1] >= long_min:
                    near.append(runs[j][0])
                    break
            for j in range(i + 1, len(runs)):
                if runs[j][1] >= long_min:
                    near.append(runs[j][0])
                    break
            if any(v == lv for lv in near):
                continue
            if any(abs(v - lv) == 1 for lv in near):
                jitter += n
            else:
                deviants += n
        legacy_ok = not qh.needs_tob_reconstruction(
            {t: v / 100.0 for t, v in r2.items()})
        return legacy_ok, deviants, jitter


def defensible_coord(sid, segments, inv_entry, mshr_records,
                     qcu_file, qcf_data, tob_bin, verbose=False):
    """Pick a DEFENSIBLE coordinate for the delivered .his using ONLY documented
    locations from the station history -- never a grid fit.

    The regime identifier is sub-cent sensitive, so it can pick a documented
    MSHR position over the inventory even when the inventory produces a
    bit-identical FINAL TOB output.  This rule (folded in from the former
    apply_defensible_coords step so ONE solver run yields the delivered set):

      1. the published INVENTORY coordinate, if it reproduces the same-or-better
         final TOB output as the identifier's choice;
      2. else the nearest documented MSHR position that does;
      3. else keep the INVENTORY and accept the <=1-cent residual -- we never
         fall back to an invented/grid-fitted coordinate.

    Returns (lat, lon, label, (legacy_ok, deviants, jitter))."""
    inv_lat, inv_lon = dms_quantize(inv_entry.lat, inv_entry.lon)
    i_ok, i_dev, i_jit = strict_validate(sid, segments, inv_lat, inv_lon,
                                         qcu_file, qcf_data, tob_bin, verbose)
    if i_dev == 0 and i_jit == 0:
        # inventory reproduces exactly -- the common case; look no further
        return inv_lat, inv_lon, "inventory", (i_ok, i_dev, i_jit)

    # Inventory leaves a residual.  Look for a documented MSHR position (nearest
    # the inventory first) that reproduces STRICTLY better -- a real recorded
    # location, never a fitted coordinate.  If none does, keep inventory and
    # accept the residual.
    inv_key = (round(inv_lat, 4), round(inv_lon, 4))
    seen = {inv_key}
    docs = []
    for rec in mshr_records:
        if rec.lat is None or rec.lon is None:
            continue
        la, lo = dms_quantize(rec.lat, rec.lon)
        key = (round(la, 4), round(lo, 4))
        if key in seen:
            continue
        seen.add(key)
        d2 = (la - inv_lat) ** 2 + (lo - inv_lon) ** 2
        docs.append((d2, f"mshr{rec.begin_date.year}", la, lo))
    docs.sort()

    best = (inv_lat, inv_lon, "inventory(residual)", (i_ok, i_dev, i_jit))
    best_score = i_dev + i_jit
    for _d, lbl, la, lo in docs:
        ok, dev, jit = strict_validate(sid, segments, la, lo, qcu_file,
                                       qcf_data, tob_bin, verbose)
        if ok and dev + jit < best_score:
            best_score = dev + jit
            best = (la, lo, lbl, (ok, dev, jit))
            if best_score == 0:
                break  # a documented move reproduces exactly
    return best


# ------------------------------------------------------------------------
# Station pipeline
# ------------------------------------------------------------------------
def process_station(sid, inv_entry, qcu_file, qcf_file, mshr_records,
                    out_dir, tob_bin, verbose=False) -> StationResult:
    qcu_data = qh.parse_station_data(qcu_file)
    qcf_data = qh.parse_station_data(qcf_file)
    R = {}
    for t, qv in qcf_data.items():
        if t in qcu_data:
            R[t] = qv - qcu_data[t]
    if not R:
        return StationResult(sid, "SKIP", detail="no residuals")
    times = sorted(R)

    cores = consensus_cores(times, R)
    regimes = chain_regimes(times, R, cores)
    if not regimes:
        return StationResult(sid, "SKIP", detail="no regimes")

    flat = all(len(set(rg.pattern.values())) == 1 for rg in regimes)
    if flat:
        # No seasonal component anywhere: single no-bias row
        start_am = qh.to_absolute_month(*times[0])
        end_am = qh.to_absolute_month(*times[-1])
        segs = [qh.TobSegment(start_am, end_am, qh.TOB_NO_BIAS_CODE, 0.0)]
        qlat, qlon = dms_quantize(inv_entry.lat, inv_entry.lon)
        write_min_his(sid, segs, qlat, qlon, out_dir)
        return StationResult(sid, "NO_TOB", n_regimes=len(regimes))

    chosen, bases = identify_regimes(
        sid, regimes, inv_entry, mshr_records, times, qcu_file, tob_bin,
        verbose)
    variant_label = chosen.label

    # Split non-exact multi-core regimes back into per-core regimes: chaining
    # of sparse cores can produce chimeric patterns (false constant-offset
    # links across an undetected TOB boundary) that match no code.
    split_regimes: List[Regime] = []
    prev_code = None
    for rg in regimes:
        if rg.exact or rg.jitter or len(rg.cores) <= 1:
            split_regimes.append(rg)
            prev_code = rg.code
            continue
        for c in rg.cores:
            pat = pattern_of(times, R, c.i0, c.i1)
            sub = Regime(cores=[Core(c.i0, c.i1, 0)], pattern=dict(pat))
            exact, jit, best = match_pattern(pat, bases)
            picked = choose_code(exact, jit, prev_code, c.i1 - c.i0 + 1)
            if picked:
                code, k, is_exact = picked
                sub.code, sub.k = code, k
                sub.exact = is_exact
                sub.jitter = not is_exact
                sub.basis = bases[code]
            elif best:
                sub.code, sub.spread, sub.k = best[0], best[1], best[2]
                sub.basis = bases[best[0]]
            else:
                continue
            split_regimes.append(sub)
            prev_code = sub.code
    if split_regimes:
        regimes = split_regimes

    # Demote short non-exact regimes: these are usually transition fragments
    # (blend months + co-located PHA steps) that the window resolver explains
    # exactly.  Keep at least one regime.
    demoted = any(rg.exact or rg.jitter for rg in regimes)
    while demoted and len(regimes) > 1:
        demoted = False
        for i, rg in enumerate(regimes):
            nobs = sum(c.i1 - c.i0 + 1 for c in rg.cores)
            if not (rg.exact or rg.jitter) and nobs <= 150:
                regimes.pop(i)
                demoted = True
                break

    n_exact = sum(1 for rg in regimes if rg.exact)
    n_hardmiss = sum(1 for rg in regimes if not (rg.exact or rg.jitter))

    # ------------------------------------------------------------------
    # Transition resolution.  seg_specs is the ordered list of
    # (start_am, start_day, code) rows for the .his; junctions between
    # regimes contribute switch events (possibly including a short
    # mid-regime that only exists inside the window).
    # ------------------------------------------------------------------
    unexplained = 0
    jitter_months = 0
    blend_errs: List[int] = []
    seg_specs: List[Tuple[int, Optional[int], str]] = []

    def blend_day_for(event_idx, left_code, right_code, off):
        res = resolve_blend_day(sid, times, R, left_code, right_code,
                                event_idx, off, chosen, qcu_file, tob_bin)
        return res  # (day, err) or None

    def emit_events(events, prev_code):
        nonlocal seg_specs
        for (eidx, code, is_blend, off) in events:
            eidx = min(eidx, len(times) - 1)
            t = times[eidx]
            start_day = None
            if is_blend:
                res = blend_day_for(eidx, prev_code, code, off)
                if res:
                    start_day, berr = res
                    if berr:
                        blend_errs.append(berr)
                else:
                    # no acceptable day: fall back to the following month
                    nxt = min(eidx + 1, len(times) - 1)
                    t = times[nxt]
            seg_specs.append((qh.to_absolute_month(*t), start_day, code))
            prev_code = code

    # leading window: months before the first core
    first = regimes[0]
    w_lo, w_hi = 0, first.cores[0].i0 - 1
    lead_start = qh.to_absolute_month(*times[0])
    if w_hi >= w_lo:
        vleft = virtual_flat_regime()
        q_idx, blend, unexp, blend_off = resolve_window(times, R, vleft, first, w_lo, w_hi)
        events = None
        if unexp > 2:
            gen = resolve_window_dp(times, R, None, first, w_lo, w_hi, bases)
            if gen is not None and gen[1] < unexp:
                events, unexp, jit = gen
                jitter_months += jit
        unexplained += unexp
        if events is not None:
            seg_specs.append((lead_start, None, qh.TOB_NO_BIAS_CODE))
            emit_events(events, qh.TOB_NO_BIAS_CODE)
        elif q_idx > w_lo and first.code != qh.TOB_NO_BIAS_CODE:
            seg_specs.append((lead_start, None, qh.TOB_NO_BIAS_CODE))
            seg_specs.append((qh.to_absolute_month(
                *times[min(q_idx, len(times) - 1)]), None, first.code))
        else:
            seg_specs.append((lead_start, None, first.code))
    else:
        seg_specs.append((lead_start, None, first.code))

    for i in range(1, len(regimes)):
        rg = regimes[i]
        prev = regimes[i - 1]
        w_lo = prev.cores[-1].i1 + 1
        w_hi = rg.cores[0].i0 - 1
        q_idx, blend, unexp, blend_off = resolve_window(times, R, prev, rg, w_lo, w_hi)
        events = None
        if unexp > 2:
            gen = resolve_window_dp(times, R, prev, rg, w_lo, w_hi, bases)
            if gen is not None and gen[1] < unexp:
                events, unexp, jit = gen
                jitter_months += jit
        unexplained += unexp
        if events is None:
            off = rg.cores[0].offset + (rg.k or 0)
            events = [(q_idx, rg.code, blend is not None, off)]
            if blend is not None:
                events = [(blend, rg.code, True,
                           blend_off if blend_off is not None else off)]
        emit_events(events, prev.code)

    # trailing window: months after the last core
    last = regimes[-1]
    w_lo, w_hi = last.cores[-1].i1 + 1, len(times) - 1
    if w_hi >= w_lo and last.code != qh.TOB_NO_BIAS_CODE:
        vright = virtual_flat_regime()
        q_idx, blend, unexp, blend_off = resolve_window(times, R, last, vright, w_lo, w_hi)
        events = None
        if unexp > 2:
            gen = resolve_window_dp(times, R, last, None, w_lo, w_hi, bases)
            if gen is not None and gen[1] < unexp:
                events, unexp, jit = gen
                jitter_months += jit
        unexplained += unexp
        if events is not None:
            emit_events(events, last.code)
        elif q_idx <= w_hi:
            seg_specs.append((qh.to_absolute_month(*times[q_idx]), None,
                              qh.TOB_NO_BIAS_CODE))

    end_am = qh.to_absolute_month(*times[-1])
    segments = assemble_segments(seg_specs, end_am)

    # Prefer a documented location (inventory, else nearest MSHR position) over
    # the identifier's coordinate whenever it reproduces the same final output,
    # so the delivered .his uses only real documented coordinates -- in one step.
    fin_lat, fin_lon, variant_label, (legacy_ok, deviants, val_jitter) = \
        defensible_coord(sid, segments, inv_entry, mshr_records, qcu_file,
                         qcf_data, tob_bin, verbose)
    write_min_his(sid, segments, fin_lat, fin_lon, out_dir)

    jitter_months += len(blend_errs)
    codes = ",".join(f"{s.tob_code}" for s in segments)
    if (legacy_ok and deviants == 0 and val_jitter == 0 and n_hardmiss == 0
            and unexplained == 0 and jitter_months == 0):
        status = "PERFECT"
    elif legacy_ok and n_hardmiss == 0 and deviants <= 6:
        # exact structure; deviants are isolated months and jitter is the
        # +/-1-cent signature of QCU re-derivation since the NOAA run —
        # neither is reproducible by any .his
        status = "NEAR"
    elif legacy_ok:
        status = "CLEAN"
    else:
        status = "FAIL"
    return StationResult(sid, status, variant_label, len(regimes), n_exact,
                         detail=(f"codes={codes} deviants={deviants} "
                                 f"jitter={val_jitter}"),
                         unexplained=unexplained)


# ------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------
def parse_args():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--inv", required=True)
    ap.add_argument("--qcu-dir", required=True)
    ap.add_argument("--qcf-dir", required=True)
    ap.add_argument("--out-history-dir", required=True)
    ap.add_argument("--mshr-zip")
    ap.add_argument("--tob-bin", required=True)
    ap.add_argument("--test-stations")
    ap.add_argument("--report", default=None,
                    help="write per-station report lines to this file")
    ap.add_argument("--verbose", action="store_true")
    return ap.parse_args()


def main():
    args = parse_args()
    tob_bin = Path(args.tob_bin).resolve()
    inventory = qh.parse_inventory(Path(args.inv))
    log(f"Loaded {len(inventory)} stations")
    if args.test_stations:
        wanted = set(s.strip() for s in args.test_stations.split(","))
        inventory = {k: v for k, v in inventory.items() if k in wanted}
        log(f"Testing {len(inventory)} stations")

    mshr = {}
    if args.mshr_zip:
        mshr = qh.read_mshr_records(Path(args.mshr_zip), set(inventory.keys()))
        log(f"Loaded MSHR for {len(mshr)} stations")

    qcu_dir, qcf_dir = Path(args.qcu_dir), Path(args.qcf_dir)
    out_dir = Path(args.out_history_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    report = open(args.report, "w") if args.report else None

    stats = Counter()
    t0 = time.time()
    for n, (sid, inv_entry) in enumerate(sorted(inventory.items()), 1):
        qcu_file = qcu_dir / f"{sid}.raw.tavg"
        qcf_file = qcf_dir / f"{sid}.qcf.tavg"
        if not qcu_file.exists() or not qcf_file.exists():
            stats["missing"] += 1
            continue
        try:
            res = process_station(sid, inv_entry, qcu_file, qcf_file,
                                  mshr.get(sid, []), out_dir, tob_bin,
                                  args.verbose)
        except Exception as e:
            import traceback
            if args.verbose:
                traceback.print_exc()
            res = StationResult(sid, "ERROR", detail=str(e))
        stats[res.status] += 1
        line = (f"{res.station_id} {res.status} variant={res.variant} "
                f"regimes={res.n_regimes} exact={res.n_exact} "
                f"unexplained={res.unexplained} {res.detail}")
        if report:
            report.write(line + "\n")
            report.flush()
        if args.verbose:
            log(line)
        if n % 100 == 0:
            el = time.time() - t0
            log(f"{n}/{len(inventory)} elapsed={el:.0f}s "
                + " ".join(f"{k}={v}" for k, v in sorted(stats.items())))

    log("SUMMARY: " + " ".join(f"{k}={v}" for k, v in sorted(stats.items())))
    if report:
        report.close()
    return 0


if __name__ == "__main__":
    sys.exit(main())
