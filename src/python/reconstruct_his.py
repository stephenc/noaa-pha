"""Station-history (.his) reconstruction driver.

Reads the prepared workspace (<base>/input/raw/tavg, <base>/output/qcf/tavg,
<base>/input/station.inv; base defaults to data/), classifies each station
(CONUS TOB gate vs PHA-only), runs the exact residual solver, writes one
JSON solution per station under work/<tag>/solutions, and renders history
files into <base>/intermediate/history:

  * CONUS TOB stations: solver-derived regime rows (bit-exact timelines).
  * CONUS COOP stations with no TOB solve: metadata-derived rows, so their
    documented moves reach PHA (--no-coop-history writes no file).
    COOP-only; see emit_coop_histories.
  * Other CONUS pure-PHA stations: NO file -- TOBMain copies the input
    verbatim when no history exists, which is numerically identical, and a
    file here would seed a PHA first-record changepoint with nothing behind
    it.
  * Non-CONUS stations NOAA is responsible for (US id, or a multi-record
    HOMR operational history) with HOMR records: metadata-derived rows
    (documented locations, elevations and observation times from
    MSHR-enhanced + PHR); TOBMain verbatim-copies these stations, the rows
    serve PHA's documented-changepoint reader.  --no-us-responsible widens
    this to every station with HOMR records.

Given a hint databank and the HOMR seed metadata, one invocation is the whole
recovery: solve, PHR fill, metadata rows and emission policy all run in-process,
with no post-passes and no TOB round-trip.

That is a statement about the pipeline, not about the result.  A regime is
proven only where QCF constrains it; a QCU month with no QCF value carries no
residual to decompose.  Vintages differ in which segments PHA retains and
removes, so a month out of reach here may be exposed in another, and donor hints
import what those vintages could prove.  The residue is documented from PHR and
HOMR -- documentation, not evidence.  More vintages widen the proven fraction;
none of them close it.

The run also provisions the pipeline's intermediate stage directory
(<base>/intermediate/, owned by this driver + TOBMain): history/, the TOB
output directory, and station.inv filtered to stations with a QCF output
file, so neither TOBMain nor PHAMain processes stations absent from the
published output set.

MSHR/PHR metadata are auto-discovered in the workspace
(<base>/mshr_enhanced.txt.zip, <base>/phr.txt.zip).

Usage:
  uv run python src/python/reconstruct_his.py            # full standard run
  uv run python src/python/reconstruct_his.py --base data-oldest
  uv run python src/python/reconstruct_his.py --class noncon --no-emit
  uv run python src/python/reconstruct_his.py --stations USC00010148
"""

from __future__ import annotations

import argparse
import json
import multiprocessing as mp
import os
import sys
from pathlib import Path
from typing import List, Optional, Tuple

sys.path.insert(0, str(Path(__file__).resolve().parent))

import ghcn_io  # noqa: E402
import his_emit  # noqa: E402
import residual_solver as rs  # noqa: E402
import tob_hints  # noqa: E402
from tob_basis import BasisRunner  # noqa: E402

REPO = Path(__file__).resolve().parents[2]
# All data-root-dependent paths (inputs, outputs, AND derived state) live
# under the --base workspace, so a run never writes outside its base dir.
# Derived state -- solutions, basis cache -- lives under <base>/intermediate;
# transient TOBMain scratch under <base>/scratch.  set_base re-roots them for
# an alternate vintage (e.g. data-oldest/).
RAW_DIR = REPO / "data" / "input" / "raw" / "tavg"
QCF_DIR = REPO / "data" / "output" / "qcf" / "tavg"
INV_PATH = REPO / "data" / "input" / "station.inv"
TOB_BIN = REPO / "bin" / "TOBMain"
CACHE_ROOT = REPO / "data" / "intermediate" / "tob_basis"
SCRATCH_ROOT = REPO / "data" / "scratch"
SOLUTIONS_DIR = REPO / "data" / "intermediate" / "solutions"
HINTS_DIR = REPO / "data" / "intermediate" / "hints"


def set_base(base: Path, work_tag: str = "") -> None:
    """Re-root all data-dependent paths at an alternate vintage workspace.

    base must contain input/raw/tavg, input/station.inv, output/qcf/tavg
    (as built by qcu_to_inputs.py/qcf_to_outputs.py --base).  Derived state
    (solutions, basis cache) goes under ``<base>/intermediate`` and transient
    scratch under ``<base>/scratch`` so a run never writes outside its base
    dir; different vintages never share caches.  ``work_tag`` is accepted for
    backward compatibility but no longer used."""
    global RAW_DIR, QCF_DIR, INV_PATH, CACHE_ROOT, SCRATCH_ROOT, SOLUTIONS_DIR
    global HINTS_DIR
    base = base.resolve()
    RAW_DIR = base / "input" / "raw" / "tavg"
    QCF_DIR = base / "output" / "qcf" / "tavg"
    INV_PATH = base / "input" / "station.inv"
    CACHE_ROOT = base / "intermediate" / "tob_basis"
    SCRATCH_ROOT = base / "scratch"
    SOLUTIONS_DIR = base / "intermediate" / "solutions"
    HINTS_DIR = base / "intermediate" / "hints"


def is_conus_gate(sid: str, lat: float, lon: float) -> bool:
    return sid.startswith("US") and 23.0 <= lat <= 50.0 and -126.0 <= lon <= -65.0


def pha_fast_path_ok(sol) -> bool:
    """Accept the PHA-only fast path for a CONUS station only when the
    solution is exact AND structurally sound: no hard-short segments
    (cost[0]) and no soft-short/audit segments (last component).  A pure-TOB
    station can otherwise masquerade as 'exact' PHA-only by fragmenting
    into hundreds of interval-feasible mini-segments."""
    return sol.exact and sol.cost[0] == 0 and sol.cost[-1] == 0


def prefer_tob(pha_sol, tob_sol) -> bool:
    """Choose between the PHA-only and TOB decompositions of one station.

    Cost tuples are not directly comparable across kinds (a fragmented
    PHA-only reading hides its badness in the changepoint component, which
    ranks BELOW tob_changes).  Rank by (deviants, hard shorts, total
    boundaries, soft shorts) -- deviants are unexplained months and outrank
    the hard-short AUDIT (which does not break exactness); otherwise a
    thousand-deviant PHA-only reading would beat a TOB solution carrying one
    audit flag.  Prefer the PHA-only reading on ties.
    """

    # A TOB no-solution sentinel (cost[0] >= 10**6) must never displace ANY
    # real PHA-only reading, however messy.
    if tob_sol.cost[0] >= 10**6:
        return False

    def rank(s):
        return (
            sum(1 for _ym, w in s.deviants if w != "flutter"),
            s.cost[0],
            s.cost[2] + s.cost[3],
            s.cost[-1],
        )

    return rank(tob_sol) < rank(pha_sol)


HIATUS_MIN_MONTHS = (
    18  # >= PHA min-segment length of ZERO data (qc-flagged data counts as data)
)


def classify_partial_months(sol, raw_values, qcf_values, mshr_recs, dataset_last):
    """Partial-month exemption rule: relabel a deviant as
    'partial-month-evidence:<...>' ONLY with evidence -- (a) it is the
    dataset-wide last month, (b) it is the station's record edge AND an
    MSHR period boundary falls strictly inside that month, or (c) it is the
    single month immediately BEFORE or AFTER a continuous hiatus of
    >= HIATUS_MIN_MONTHS calendar months with zero data in the raw record
    (a month present with any value, even QC-flagged, breaks the hiatus) --
    the trailing/leading month of a collection stop/restart is typically a
    partial month.  Everything else stays a regular deviant."""
    if not sol.deviants:
        return
    common = sorted(set(raw_values) & set(qcf_values))
    if not common:
        return
    edges = {common[0], common[-1]}
    # Hiatus adjacency computed on the RAW record: months with any value
    # present (even QC-flagged) count as data.
    raw_mis = sorted(y * 12 + (m - 1) for (y, m) in raw_values)
    hiatus_adjacent = {}  # month-index -> gap length (calendar months)
    for a, b in zip(raw_mis, raw_mis[1:]):
        gap = b - a - 1
        if gap >= HIATUS_MIN_MONTHS:
            hiatus_adjacent[a] = gap  # single trailing month before the gap
            hiatus_adjacent[b] = gap  # single leading month after the gap
    relabeled = []
    for ym, why in sol.deviants:
        ymt = tuple(ym)
        mi = ymt[0] * 12 + (ymt[1] - 1)
        evidence = None
        if dataset_last is not None and ymt == tuple(dataset_last):
            evidence = "dataset-last"
        elif mi in hiatus_adjacent:
            evidence = f"hiatus-{hiatus_adjacent[mi]}mo"
        elif ymt in edges:
            for rec in mshr_recs or []:
                for b in (rec.begin, rec.end):
                    if (
                        b is not None
                        and (b[0], b[1]) == ymt
                        and 1 < b[2] < _days_in_month(b[0], b[1])
                    ):
                        evidence = f"{b[0]:04d}{b[1]:02d}{b[2]:02d}"
                        break
                if evidence:
                    break
        if evidence is not None:
            relabeled.append((ymt, f"partial-month-evidence:{evidence}"))
        else:
            relabeled.append((ym, why))
    sol.deviants = relabeled


def _days_in_month(y: int, m: int) -> int:
    if m == 2:
        leap = y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)
        return 29 if leap else 28
    return [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1]


def solve_station(
    sid: str,
    emit_his: bool = False,
    his_dir: Optional[Path] = None,
) -> Tuple[str, str]:
    """Solve one station; returns (sid, one-line summary)."""
    raw_path = RAW_DIR / f"{sid}.raw.tavg"
    qcf_path = QCF_DIR / f"{sid}.qcf.tavg"
    if not raw_path.exists():
        return sid, f"{sid}\tSKIP\tno-raw"
    inv_all = read_inventory_cached()
    inv = inv_all.get(sid)
    if inv is None:
        return sid, f"{sid}\tSKIP\tno-inventory"
    raw = ghcn_io.read_station_data(raw_path)
    if not qcf_path.exists():
        return sid, f"{sid}\tEXCLUDED\tnot-in-qcf"
    qcf = ghcn_io.read_station_data(qcf_path)

    conus = is_conus_gate(sid, inv.lat, inv.lon)
    sol = rs.solve_pha_only(raw.values, qcf.values, raw.flags, sid=sid)
    kind = "pha-only"
    # Every station solves at its published station.inv coordinate (verified
    # across both data vintages).  MSHR data is used only for partial-month
    # evidence classification.
    coords, prov = [(inv.lat, inv.lon)], ["inventory"]
    inv_bases = None  # chosen-coord bases, when the TOB gate ran (for §6.4)

    # Donor hints (loaded once, reused for consolidation).  By default (Phase
    # 2, §9) they also influence the SOLVE via vintage_hints; with
    # --no-vintage-hints they act only at consolidation, outside the QCF hull.
    hint_sets = []
    if _HINT_DIRS and conus:
        hint_sets = tob_hints.load_hint_sets(
            _HINT_DIRS, sid, solver_version=rs.SOLVER_EVIDENCE_VERSION, log=print
        )
    vintage_hints = (
        tob_hints.vintage_hints_from_sets(hint_sets)
        if (_VINTAGE_HINTS_ON and hint_sets)
        else None
    )

    if conus and not pha_fast_path_ok(sol):
        runner = BasisRunner(TOB_BIN, SCRATCH_ROOT, CACHE_ROOT)

        _blend_memo: dict = {}

        def blend_fn(coord, ym, c_old, c_new):
            # One TOBMain blend run per distinct (coord, month, code pair)
            # per station -- the search may re-ask during repairs/refinement.
            key = (coord, ym, c_old, c_new)
            if key not in _blend_memo:
                y, m = ym
                _blend_memo[key] = runner.blend_offsets(
                    sid,
                    raw_path,
                    coord,
                    (y, m),
                    c_old,
                    c_new,
                    range(2, _days_in_month(y, m) + 1),
                )
            return _blend_memo[key]

        hints = None
        if _PHR_CACHE is not None:
            import metadata_accuracy as _ma

            events, _map = _ma.meta_events(_PHR_CACHE.get(sid, []))
            hints = [
                (ev.date, code) for ev in events for code in (ev.codes_after or ())
            ]

        # The solver receives the inventory-coordinate basis (27 fakes).
        # When that basis is unusable (e.g. mict<5), the solver still runs
        # so it produces its no-solution reading.
        inv_bases = runner.get_bases(sid, raw_path, coords)
        tob_sol = rs.solve_tob_station(
            raw.values,
            qcf.values,
            inv_bases,
            blend_fn,
            raw.flags,
            sid=sid,
            hints=hints,
            vintage_hints=vintage_hints,
        )
        if prefer_tob(sol, tob_sol):
            sol = tob_sol
            kind = "tob"
        else:
            # A messy-but-solved PHA-only reading must never lose to the TOB
            # search's no-solution sentinel (or a worse TOB reading): keep
            # the PHA-only solution, audit-flagged.
            sol.audits.append("pha-only-despite-shorts")

    classify_partial_months(
        sol,
        raw.values,
        qcf.values,
        _MSHR_CACHE.get(sid) if _MSHR_CACHE else None,
        _DATASET_LAST,
    )

    # Chosen coordinate + provenance (verify_his resolves bases from these).
    ci = sol.coord_index if (kind == "tob" and sol.coord_index is not None) else 0
    chosen_coord = coords[ci]
    chosen_prov = prov[ci]

    SOLUTIONS_DIR.mkdir(parents=True, exist_ok=True)
    d = sol.to_dict()
    d["coord"] = list(chosen_coord)
    d["coord_provenance"] = chosen_prov

    # Evidence export (§4.6): derived from the PURE residual solve, before any
    # consolidation.  CONUS stations (TOB and pha-only) get a hints file; the
    # class is computed here from the post-relabel deviants (classify_partial_
    # months has already run).  --no-hints-out suppresses this.
    if conus and not _NO_HINTS_OUT:
        hints_obj = tob_hints.hints_from_solution(
            sid, d, raw.values, qcf.values, base_name=_BASE_NAME, stamp=_RUN_STAMP
        )
        tob_hints.write_station_hints(HINTS_DIR, hints_obj)

    # Consolidation (§6.6): extend the timeline with donor hints outside the
    # QCF hull.  Runs only with --hints, on CONUS stations, and never touches
    # the pure residual solve (hints already exported above).  Promotion of an
    # exact pha-only station is flag-gated and re-kinds the LOCAL `kind` so the
    # emission gate + summary see "tob".
    if _HINT_DIRS and conus:
        # hint_sets already loaded before the solve (reused here).
        eligible = kind == "tob" or (
            kind == "pha-only" and sol.exact and _PROMOTE_PHA_ONLY
        )
        if hint_sets and eligible:
            cur_offsets = None
            try:
                if inv_bases and ci < len(inv_bases):
                    cur_offsets = inv_bases[ci].code_offsets_by_ym
                else:
                    _r = BasisRunner(TOB_BIN, SCRATCH_ROOT, CACHE_ROOT)
                    _b = _r.get_bases(sid, raw_path, [chosen_coord])
                    cur_offsets = _b[0].code_offsets_by_ym if _b else None
            except Exception:  # pragma: no cover - basis unavailable
                cur_offsets = None
            cons = tob_hints.consolidate(
                d["regimes"],
                kind,
                hint_sets,
                raw.values,
                qcf.values,
                current_offsets=cur_offsets,
                promote_pha_only=_PROMOTE_PHA_ONLY,
            )
            if cons.adopted_pre or cons.adopted_post:
                d["residual_regimes"] = d["regimes"]  # pure solve preserved
                d["regimes"] = cons.regimes  # consolidated plain dicts
                d["audits"].append(
                    f"hints-consolidated:pre={cons.adopted_pre},"
                    f"post={cons.adopted_post}"
                )
                d["hint_consolidation"] = {
                    "source_kind": kind,  # pre-promotion kind (for idempotent recompute)
                    "notes": cons.notes,
                    "refusals": cons.refusals,
                }
                if cons.promoted:
                    d["kind"] = "tob"
                    kind = "tob"  # LOCAL: gates emission + summary below
                    d["audits"].append("hint-promoted-pha-only")

    # Atomic write: --skip-existing resumes on existence alone, so a run
    # killed mid-write must never leave a truncated {sid}.json that resume
    # then treats as done.  Write to a sibling temp and rename (rename is
    # atomic within a directory).
    final = SOLUTIONS_DIR / f"{sid}.json"
    tmp = SOLUTIONS_DIR / f"{sid}.json.tmp"
    with open(tmp, "w") as fh:
        json.dump(d, fh, indent=1)
    os.replace(tmp, final)

    # Pure-PHA CONUS stations get NO .his file here: TOBMain copies the input
    # verbatim when no history exists (numerically identical, byte-safe), and
    # an artificial 00HR-only file would seed a PHA first-record changepoint
    # with no documented event behind it.  00HR rows appear only where a
    # TOB solve genuinely required bracketing (his_emit's first-row rule).
    # Rows carry the CHOSEN coordinate (constant across rows -> no spurious
    # PHA changepoints; correct for tob.use-his-lat-lon=true validation runs).
    # Non-CONUS stations with HOMR records get a METADATA-derived history
    # (documented locations/elevations/obs times); TOBMain verbatim-copies
    # them regardless, so TOB outputs are unaffected -- the rows exist for
    # PHA's documented-changepoint reader.
    # Which stations get a metadata history is a US-RESPONSIBILITY question,
    # not a geographic one: NOAA maintains US-territory and overseas-base
    # stations too, and HOMR holds a multi-record operational history for
    # exactly those.  A single-record foreign station yields a flat stub that
    # documents nothing and costs PHA a first-record changepoint.  The rule
    # reads only the id and the HOMR record count -- independent of QCF and of
    # whether any changepoint results -- so it cannot be tuned against the
    # target (--no-us-responsible emits for every station with HOMR records).
    # The CONUS gate implies a US id, so this only prunes the non-CONUS branch.
    phr_recs = _PHR_CACHE.get(sid) if _PHR_CACHE else None
    mshr_recs = _MSHR_CACHE.get(sid) if _MSHR_CACHE else None
    us_responsible = (
        not _US_RESPONSIBLE_ONLY or sid.startswith("US") or len(mshr_recs or ()) > 1
    )
    if emit_his and conus and kind == "tob" and d["regimes"]:
        out_dir = his_dir or (REPO / "work" / "history")
        first_year = raw.years[0] if raw.years else 1895
        # Build from d["regimes"] (dicts): after consolidation/promotion these
        # are the consolidated timeline, NOT sol.regimes (§6.6).
        regimes = [
            his_emit.Regime(begin=tuple(r["begin"]), obs_time=r["code"])
            for r in d["regimes"]
        ]
        dms = ghcn_io.dms_quantize(*chosen_coord)
        his_emit.emit_station_his(sid, regimes, dms, inv, out_dir, first_year)
    elif emit_his and not conus and us_responsible:
        if phr_recs or mshr_recs:
            out_dir = his_dir or (REPO / "work" / "history")
            his_emit.emit_metadata_his(
                sid,
                phr_recs,
                mshr_recs,
                inv,
                out_dir,
                first_data=min(raw.values) if raw.values else None,
            )

    n_reg = len(d["regimes"])
    n_cp = max(0, len(sol.segments) - 1)
    summary = (
        f"{sid}\t{kind}\texact={sol.exact}\tregimes={n_reg}\tcps={n_cp}"
        f"\tdeviants={len(sol.deviants)}\tcost={sol.cost}"
    )
    return sid, summary


def emit_coop_histories(base: Path, his_dir: Path) -> None:
    """Give CONUS COOP stations with no TOB solve a metadata history.

    The solve emits solver-derived rows for a CONUS station with a TOB timeline
    and metadata rows for a non-CONUS one, so a CONUS station with neither
    matches no branch.  Without this pass its documented moves never reach PHA.
    Emission goes through the same ``emit_metadata_his`` path non-CONUS stations
    use, so the two are treated alike.

    COOP only: USR/USS/USW records are longer, denser and carry appreciably
    more changepoints each, so undated-provenance rows give PHA more
    opportunities to split a segment that should stay whole, where the shorter,
    sparser COOP records mostly cannot.

    Ordering is load-bearing: this runs after the PHR fill and the metadata
    rows.  Those passes repair the fields ``emit_station_his`` writes flat; the
    files written here are HOMR-derived already, and putting them through the
    same passes would rewrite fields that are correct as emitted.

    These stations sit INSIDE the TOB gate, so the history must span the whole
    record (``cover_leading_months``).  TOBMain applies sunset, not midnight, to
    months no record covers, and HOMR documentation routinely begins after the
    data does -- an uncovered leading span would therefore carry a TOB
    adjustment.  Every station reaching this function solved as pha-only, which
    is a positive finding of NO adjustment anywhere in its record, so such a
    span would contradict the residual evidence.
    """
    inv = ghcn_io.read_inventory(base / "intermediate" / "station.inv")
    have = {p.name.split(".")[0] for p in his_dir.glob("*.his")}
    missing = sorted(
        s
        for s in inv
        if s.startswith("USC")
        and s not in have
        and (RAW_DIR / f"{s}.raw.tavg").is_file()
    )
    mshr = ghcn_io.read_mshr(base / "mshr_enhanced.txt.zip", set(missing))
    phr = ghcn_io.read_phr(base / "phr.txt.zip", set(missing))
    written = skipped = 0
    for sid in missing:
        m, p = mshr.get(sid), phr.get(sid)
        if not m and not p:
            skipped += 1
            continue
        vals = ghcn_io.read_station_data(RAW_DIR / f"{sid}.raw.tavg").values
        if not vals:
            skipped += 1
            continue
        out = his_emit.emit_metadata_his(
            sid,
            p,
            m,
            inv[sid],
            his_dir,
            first_data=min(vals),
            cover_leading_months=True,
        )
        if out is None:
            skipped += 1
        else:
            written += 1
    print(
        "# COOP histories written: %d (candidates %d, skipped %d)"
        % (written, len(missing), skipped),
        flush=True,
    )


_INV_CACHE = None
_MSHR_CACHE: Optional[dict] = None
_PHR_CACHE: Optional[dict] = None
_DATASET_LAST: Optional[Tuple[int, int]] = None
# Set in main() before the fork pool; inherited copy-on-write by workers.
_NO_HINTS_OUT: bool = False
_US_RESPONSIBLE_ONLY: bool = True
_BASE_NAME: str = "data"
_RUN_STAMP: str = ""
# Consolidation (Stage 4): donor hint dirs (--hints) and pha-only promotion.
_HINT_DIRS: List[Path] = []
_PROMOTE_PHA_ONLY: bool = False
# Phase 2 (§9): let donor hints influence the SOLVE.  ON by default when
# --hints is given; disabled with --no-vintage-hints.
_VINTAGE_HINTS_ON: bool = True


def _dataset_last_month(qcf_dir: Path, sample: int = 40) -> Optional[Tuple[int, int]]:
    """Dataset-wide last (current, typically partial) month, from a sample."""
    import itertools as _it

    last: Optional[Tuple[int, int]] = None
    for p in _it.islice(sorted(qcf_dir.glob("*.qcf.tavg")), sample):
        try:
            sd = ghcn_io.read_station_data(p)
        except Exception:
            continue
        if sd.values:
            m = max(sd.values)
            if last is None or m > last:
                last = m
    return last


def read_inventory_cached():
    global _INV_CACHE
    if _INV_CACHE is None:
        _INV_CACHE = ghcn_io.read_inventory(INV_PATH)
    return _INV_CACHE


def _select_stations(args) -> List[str]:
    if args.stations:
        return [s.strip() for s in args.stations.split(",") if s.strip()]
    if args.station_file:
        return [
            line.strip()
            for line in open(args.station_file)
            if line.strip() and not line.startswith("#")
        ]
    inv = read_inventory_cached()
    ids = []
    for sid, rec in inv.items():
        raw = RAW_DIR / f"{sid}.raw.tavg"
        if not raw.exists():
            continue
        gate = is_conus_gate(sid, rec.lat, rec.lon)
        if args.station_class == "all":
            ids.append(sid)
        elif args.station_class == "conus" and gate:
            ids.append(sid)
        elif args.station_class == "noncon" and not gate:
            ids.append(sid)
    return sorted(ids)


def _worker(job) -> Tuple[str, str]:
    sid, emit_his, his_dir = job
    try:
        return solve_station(sid, emit_his, Path(his_dir) if his_dir else None)
    except Exception as exc:  # pragma: no cover - defensive per-station guard
        return sid, f"{sid}\tERROR\t{type(exc).__name__}: {exc}"


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    sel = ap.add_mutually_exclusive_group()
    sel.add_argument("--stations", help="comma-separated station ids")
    sel.add_argument("--station-file", help="file with one station id per line")
    sel.add_argument(
        "--class",
        dest="station_class",
        choices=["all", "conus", "noncon"],
        default="all",
        help="station class to process (default: all)",
    )
    ap.add_argument(
        "--no-emit",
        action="store_true",
        help="analysis-only run: write solution JSONs but no .his files",
    )
    ap.add_argument(
        "--his-dir",
        default=None,
        help="history output directory (default: <base>/intermediate/history)",
    )
    ap.add_argument(
        "--mshr-zip",
        default=None,
        help="MSHR metadata zip (default: <base>/mshr_enhanced.txt.zip when "
        "present); feeds partial-month evidence classification and "
        "non-CONUS metadata histories",
    )
    ap.add_argument(
        "--phr-zip",
        default=None,
        help="PHR zip (default: <base>/phr.txt.zip when present); obs-time "
        "change events are search HINTS (candidate ranking only -- never "
        "constraints; results stay bit-exact) and non-CONUS history rows",
    )
    ap.add_argument(
        "--jobs",
        type=int,
        default=max(1, (os.cpu_count() or 2) - 1),
        help="worker processes (default: cpu_count - 1)",
    )
    ap.add_argument(
        "--summary-file",
        default=None,
        help="summary TSV path (default: work/<tag>/summary.tsv)",
    )
    ap.add_argument(
        "--base",
        default="data",
        help="workspace root as built by qcu_to_inputs/qcf_to_outputs --base "
        "(default: data); derived state goes under work/<basename>/",
    )
    ap.add_argument(
        "--skip-existing",
        action="store_true",
        help="skip stations whose solution JSON already exists (resume an "
        "interrupted batch; only sound when the existing solutions come "
        "from the same solver version and workspace)",
    )
    ap.add_argument(
        "--hints",
        action="append",
        default=None,
        metavar="DIR",
        help="donor hints directory (repeatable, e.g. a hintstore vintage or "
        "data-oldest/intermediate/hints).  Hints influence the solve and "
        "extend timelines outside this vintage's QCF hull at consolidation; "
        "--no-vintage-hints limits them to the latter",
    )
    ap.add_argument(
        "--promote-pha-only",
        action="store_true",
        help="allow a CONUS exact pha-only station with adoptable exterior "
        "hint regimes to emit a 24HR-hull + exteriors timeline (re-kinded "
        "'tob'); default off (emitting any .his reverses a driver policy)",
    )
    ap.add_argument(
        "--no-hints-out",
        action="store_true",
        help="suppress writing this run's own hints files "
        "(<base>/intermediate/hints)",
    )
    ap.add_argument(
        "--no-vintage-hints",
        action="store_true",
        help="Phase 2 (§9) is ON by default when --hints is given: donor "
        "hints INFLUENCE the solve (enumeration preference + hinted-boundary "
        "retry), and policy adoptions export as class residual-proven-hinted "
        "(never re-adoptable).  Pass this flag to disable it, leaving hints to "
        "act only at consolidation, outside the QCF hull",
    )
    # --- post-solve passes (on by default; see the module docstring) ---------
    ap.add_argument(
        "--phr-fill-regions",
        default="pre,interior,post",
        help="which residual-unconstrained regions PHR may document: any "
        "comma-separated subset of pre,interior,post (default: all).  PHR "
        "never overrides the current vintage's solve or an adopted hint",
    )
    ap.add_argument(
        "--no-phr-fill",
        action="store_true",
        help="skip the PHR observation-time fill, leaving residual-"
        "unconstrained months to carry the synthetic 00HR pad",
    )
    ap.add_argument(
        "--no-metadata-rows",
        action="store_true",
        help="keep the flattened non-TOB .his fields.  By default coordinates, "
        "elevation, relocations and instruments are taken from HOMR so that a "
        "PHA run with use-history-files=1 sees the documented changepoints; "
        "pass this for a strictly TOB-only history",
    )
    ap.add_argument(
        "--no-us-responsible",
        action="store_true",
        help="emit a metadata history for EVERY non-CONUS station with HOMR "
        "records.  By default emission is restricted to the stations NOAA is "
        "responsible for (US id, or a multi-record HOMR operational history, "
        "which is what US territories and the overseas base network have); the "
        "rest yield flat stubs that document nothing and cost PHA a "
        "first-record changepoint",
    )
    ap.add_argument(
        "--no-coop-history",
        action="store_true",
        help="leave CONUS COOP stations with no TOB solve without a history "
        "file.  By default they get one from HOMR via the same path non-CONUS "
        "stations use, so their documented moves reach PHA; COOP-only, because "
        "the longer, denser USR/USS/USW records give PHA more opportunities to "
        "split a segment that should stay whole",
    )
    args = ap.parse_args()

    # --skip-existing resumes skip solve_station entirely, so consolidation
    # would produce mixed outputs; refuse the combination (§6.6).
    if args.skip_existing and args.hints:
        ap.error("--skip-existing is incompatible with --hints")

    base = Path(args.base)
    tag = base.name
    # Always re-root under base (incl. base=data) so nothing is written
    # outside the base dir on any invocation.
    set_base(base, tag)
    if args.his_dir is None:
        args.his_dir = str(base / "intermediate" / "history")
    if args.summary_file is None:
        args.summary_file = str(base / "intermediate" / "summary.tsv")
    # A metadata path given EXPLICITLY on the command line must exist.  On a
    # typo'd or absent path the run would otherwise fall through to the "no
    # evidence" branch and still exit 0, which degrades the whole pass: no
    # partial-month evidence for the CONUS solve, and no metadata histories at
    # all.  Auto-discovery below may find nothing, because it only ever assigns
    # a path that already exists.
    for flag, value in (("--mshr-zip", args.mshr_zip), ("--phr-zip", args.phr_zip)):
        if value is not None and not Path(value).is_file():
            sys.exit("%s: no such file: %s" % (flag, value))
    if args.mshr_zip is None and (base / "mshr_enhanced.txt.zip").exists():
        args.mshr_zip = str(base / "mshr_enhanced.txt.zip")
    if args.phr_zip is None and (base / "phr.txt.zip").exists():
        args.phr_zip = str(base / "phr.txt.zip")

    stations = _select_stations(args)
    if not args.no_emit:
        # The intermediate (TOB-stage) station list: input/station.inv
        # filtered to stations that have a QCF output file.  TOBMain and
        # PHAMain iterate this list, so stations absent from the published
        # output set are never processed into the intermediate or the
        # adjusted output.
        qcf_ids = {p.name.split(".")[0] for p in QCF_DIR.glob("*.qcf.tavg")}
        intermediate = INV_PATH.parent.parent / "intermediate"
        inv_out = intermediate / "station.inv"
        # TOBMain requires its output directory to pre-exist; history is
        # also intermediate-owned (reconstructed, not published inputs).
        (intermediate / "tob" / "tavg").mkdir(parents=True, exist_ok=True)
        Path(args.his_dir).mkdir(parents=True, exist_ok=True)
        kept = 0
        with open(INV_PATH) as src, open(inv_out, "w") as dst:
            for line in src:
                if line[:11] in qcf_ids:
                    dst.write(line)
                    kept += 1
        print(f"# intermediate inv: {kept} stations -> {inv_out}", flush=True)
    if args.skip_existing:
        before = len(stations)
        stations = [s for s in stations if not (SOLUTIONS_DIR / f"{s}.json").exists()]
        print(
            f"# resume: skipping {before - len(stations)} existing solutions",
            flush=True,
        )
    # Hints export + consolidation config, set before the fork pool so
    # workers inherit it copy-on-write (like the PHR/MSHR caches).
    global _NO_HINTS_OUT, _BASE_NAME, _RUN_STAMP, _HINT_DIRS, _PROMOTE_PHA_ONLY
    global _VINTAGE_HINTS_ON, _US_RESPONSIBLE_ONLY
    _NO_HINTS_OUT = args.no_hints_out
    _US_RESPONSIBLE_ONLY = not args.no_us_responsible
    _BASE_NAME = tag
    _PROMOTE_PHA_ONLY = args.promote_pha_only
    _VINTAGE_HINTS_ON = not args.no_vintage_hints
    import datetime as _dt

    _RUN_STAMP = _dt.datetime.now(_dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    if not _NO_HINTS_OUT:
        HINTS_DIR.mkdir(parents=True, exist_ok=True)
        print(f"# hints out: {HINTS_DIR}", flush=True)
    if args.hints:
        active = HINTS_DIR.resolve()
        hint_dirs: List[Path] = []
        for h in args.hints:
            hp = Path(h).resolve()
            if hp == active:
                ap.error(
                    f"--hints {h} resolves to this run's own hints dir "
                    "(self-hints forbidden: output must be a pure function of "
                    "the workspace + DONOR hints)"
                )
            if not hp.is_dir():
                ap.error(f"--hints {h}: not a directory")
            hint_dirs.append(hp)
        _HINT_DIRS = hint_dirs
        print(
            f"# consolidation: {len(_HINT_DIRS)} hint dir(s); "
            f"promote_pha_only={_PROMOTE_PHA_ONLY}",
            flush=True,
        )

    global _DATASET_LAST
    _DATASET_LAST = _dataset_last_month(QCF_DIR)
    if args.phr_zip:
        global _PHR_CACHE
        _PHR_CACHE = ghcn_io.read_phr(Path(args.phr_zip), set(stations))
    else:
        print("# phr.txt.zip not found: no search hints / non-CONUS obs rows")
    if args.mshr_zip:
        global _MSHR_CACHE
        _MSHR_CACHE = ghcn_io.read_mshr(Path(args.mshr_zip), set(stations))
        print(
            f"# mshr evidence: records for "
            f"{sum(1 for s in stations if _MSHR_CACHE.get(s))}/{len(stations)} "
            "stations",
            flush=True,
        )
    else:
        print("# mshr_enhanced.txt.zip not found: no partial-month evidence")
    jobs = [(sid, not args.no_emit, args.his_dir) for sid in stations]
    lines: List[str] = []
    if args.jobs > 1:
        with mp.get_context("fork").Pool(args.jobs) as pool:
            for _sid, line in pool.imap_unordered(_worker, jobs, chunksize=4):
                print(line, flush=True)
                lines.append(line)
    else:
        for job in jobs:
            _sid, line = _worker(job)
            print(line, flush=True)
            lines.append(line)

    n_exact = sum("exact=True" in l for l in lines)
    print(f"# {n_exact}/{len(lines)} exact", flush=True)
    if args.summary_file:
        Path(args.summary_file).parent.mkdir(parents=True, exist_ok=True)
        # Sort by station id (each line starts with the sid): imap_unordered
        # yields in completion order, so an unsorted summary of a bit-exact
        # pipeline would differ byte-for-byte between runs.
        with open(args.summary_file, "w") as fh:
            fh.write("\n".join(sorted(lines)) + "\n")

    # ---- post-passes over the emitted timeline -------------------------------
    # These run in-process so `reconstruct_his.py --base <dir>` stays the single
    # command that turns QCU + QCF + metadata (+ donor hints) into histories.
    # Each is idempotent and reads only what the solve already wrote, so any
    # can be re-run alone against a finished base to retune policy without
    # repeating the (hours-long) solve.
    if args.no_emit:
        return

    if not args.no_phr_fill:
        import phr_fill

        print("# --- PHR fill (regions=%s) ---" % args.phr_fill_regions, flush=True)
        rc = phr_fill.main(
            [
                "--base",
                str(base),
                "--regions",
                args.phr_fill_regions,
                "--report",
                str(base / "intermediate" / "phr_fill.tsv"),
            ]
        )
        if rc != 0:
            raise SystemExit(f"phr fill failed (rc={rc})")

    if not args.no_metadata_rows:
        import his_metadata

        print("# --- metadata rows (HOMR non-TOB fields) ---", flush=True)
        rc = his_metadata.main(
            [
                "--base",
                str(base),
                "--report",
                str(base / "intermediate" / "his_metadata.tsv"),
            ]
        )
        if rc != 0:
            raise SystemExit(f"metadata rows failed (rc={rc})")

    if not args.no_coop_history:
        print("# --- COOP histories (CONUS, no TOB solve) ---", flush=True)
        emit_coop_histories(base, Path(args.his_dir))


if __name__ == "__main__":
    main()
