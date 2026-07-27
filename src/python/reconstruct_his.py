"""Station-history (.his) reconstruction driver.

Reads the prepared workspace (<base>/input/raw/tavg, <base>/output/qcf/tavg,
<base>/input/station.inv; base defaults to data/), classifies each station
(CONUS TOB gate vs PHA-only), runs the exact residual solver, writes one
JSON solution per station under work/<tag>/solutions, and renders history
files into <base>/intermediate/history:

  * CONUS TOB stations: solver-derived regime rows (bit-exact timelines).
  * CONUS pure-PHA stations: NO file -- TOBMain copies the input verbatim
    when no history exists, which is numerically identical and avoids
    seeding a PHA first-record changepoint that NOAA's pipeline never had.
  * Non-CONUS stations with HOMR records: metadata-derived rows (documented
    locations, elevations and observation times from MSHR-enhanced + PHR);
    TOBMain verbatim-copies these stations, the rows serve PHA's
    documented-changepoint reader.

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
import sys
from pathlib import Path
from typing import List, Optional, Tuple

sys.path.insert(0, str(Path(__file__).resolve().parent))

import ghcn_io  # noqa: E402
import his_emit  # noqa: E402
import residual_solver as rs  # noqa: E402
from tob_basis import BasisRunner  # noqa: E402

REPO = Path(__file__).resolve().parents[2]
# Data-root-dependent paths are module-level for historical reasons; the
# --base CLI option (set_base) re-roots them so an alternate vintage
# workspace (e.g. data-oldest/) can be solved standalone.
RAW_DIR = REPO / "data" / "input" / "raw" / "tavg"
QCF_DIR = REPO / "data" / "output" / "qcf" / "tavg"
INV_PATH = REPO / "data" / "input" / "station.inv"
TOB_BIN = REPO / "bin" / "TOBMain"
CACHE_ROOT = REPO / "work" / "tob_basis"
SCRATCH_ROOT = REPO / "work" / "tob_scratch"
SOLUTIONS_DIR = REPO / "work" / "solutions"


def set_base(base: Path, work_tag: str) -> None:
    """Re-root all data-dependent paths at an alternate vintage workspace.

    base must contain input/raw/tavg, input/station.inv, output/qcf/tavg
    (as built by qcu_to_inputs.py/qcf_to_outputs.py --base).  Derived state
    (basis cache, scratch, solutions) moves under work/<work_tag>/ so
    vintages never share caches."""
    global RAW_DIR, QCF_DIR, INV_PATH, CACHE_ROOT, SCRATCH_ROOT, SOLUTIONS_DIR
    base = base.resolve()
    RAW_DIR = base / "input" / "raw" / "tavg"
    QCF_DIR = base / "output" / "qcf" / "tavg"
    INV_PATH = base / "input" / "station.inv"
    CACHE_ROOT = REPO / "work" / work_tag / "tob_basis"
    SCRATCH_ROOT = REPO / "work" / work_tag / "tob_scratch"
    SOLUTIONS_DIR = REPO / "work" / work_tag / "solutions"


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
    sol = rs.solve_pha_only(raw.values, qcf.values, qcf.flags, sid=sid)
    kind = "pha-only"
    # Every station solves at its published station.inv coordinate (verified
    # across both data vintages).  MSHR data is used only for partial-month
    # evidence classification.
    coords, prov = [(inv.lat, inv.lon)], ["inventory"]
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
            qcf.flags,
            sid=sid,
            hints=hints,
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
    with open(SOLUTIONS_DIR / f"{sid}.json", "w") as fh:
        json.dump(d, fh, indent=1)

    # Pure-PHA CONUS stations get NO .his file: TOBMain copies the input
    # verbatim when no history exists (numerically identical, byte-safe),
    # and an artificial 00HR-only file would seed a PHA first-record
    # changepoint NOAA's pipeline never had.  00HR rows appear only where a
    # TOB solve genuinely required bracketing (his_emit's first-row rule).
    # Rows carry the CHOSEN coordinate (constant across rows -> no spurious
    # PHA changepoints; correct for tob.use-his-lat-lon=true validation runs).
    # Non-CONUS stations with HOMR records get a METADATA-derived history
    # (documented locations/elevations/obs times); TOBMain verbatim-copies
    # them regardless, so TOB outputs are unaffected -- the rows exist for
    # PHA's documented-changepoint reader.
    if emit_his and conus and kind == "tob" and sol.regimes:
        out_dir = his_dir or (REPO / "work" / "history")
        first_year = raw.years[0] if raw.years else 1895
        regimes = [his_emit.Regime(begin=r.begin, obs_time=r.code) for r in sol.regimes]
        dms = ghcn_io.dms_quantize(*chosen_coord)
        his_emit.emit_station_his(sid, regimes, dms, inv, out_dir, first_year)
    elif emit_his and not conus:
        phr_recs = _PHR_CACHE.get(sid) if _PHR_CACHE else None
        mshr_recs = _MSHR_CACHE.get(sid) if _MSHR_CACHE else None
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

    n_reg = len(sol.regimes)
    n_cp = max(0, len(sol.segments) - 1)
    summary = (
        f"{sid}\t{kind}\texact={sol.exact}\tregimes={n_reg}\tcps={n_cp}"
        f"\tdeviants={len(sol.deviants)}\tcost={sol.cost}"
    )
    return sid, summary


_INV_CACHE = None
_MSHR_CACHE: Optional[dict] = None
_PHR_CACHE: Optional[dict] = None
_DATASET_LAST: Optional[Tuple[int, int]] = None


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
    import os

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
    args = ap.parse_args()

    base = Path(args.base)
    tag = base.name
    if base.resolve() != (REPO / "data").resolve():
        set_base(base, tag)
    if args.his_dir is None:
        args.his_dir = str(base / "intermediate" / "history")
    if args.summary_file is None:
        args.summary_file = str(REPO / "work" / tag / "summary.tsv")
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
        with open(args.summary_file, "w") as fh:
            fh.write("\n".join(lines) + "\n")


if __name__ == "__main__":
    main()
