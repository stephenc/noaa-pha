"""Full-pipeline verification gate.

Verifies the pipeline artifacts AFTER a TOBMain run over the emitted
histories:

  PHA side: for every non-deviant month covered by a solved segment,
      qcf(y,m) == fp32.pha_qcf(t_out(y,m), S)
    must hold BIT-EXACTLY for S at the solved interval's midpoint AND both
    endpoints.  If the endpoints disagree with each other, the stored
    interval is stale relative to t_out -- the segment's S is re-solved from
    the (t_out, qcf) pairs (fp32.solve_segment) and the station is flagged
    'resegmented' (not a failure by itself).

  TOB side: t_out(y,m) - qcu(y,m) must equal the solver's implied tobo for
    the month's regime code (recomputed from the RAW basis at the
    solution's coordinate; knife_edges entries are documentation-only
    compiler-divergence candidates and are never applied).  Any mismatch is
    emission or parsing infidelity -- exactly what this gate exists to
    catch.  Months whose regime begins mid-month (blend months) are exempt
    from the TOB equality (they are still verified on the PHA side via
    t_out).

  Verbatim side: stations WITHOUT an emitted .his (non-CONUS, and CONUS
    pure-PHA per the driver policy) must have t_out identical to qcu.

Usage (after the full TOBMain run):
  uv run python src/python/verify_his.py --report work/verification_report.tsv \
      --jobs 8
"""

from __future__ import annotations

import argparse
import json
import multiprocessing as mp
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Dict, List, Optional, Tuple

sys.path.insert(0, str(Path(__file__).resolve().parent))

import fp32  # noqa: E402
import ghcn_io  # noqa: E402

REPO = Path(__file__).resolve().parents[2]
DEF_TOB_DIR = REPO / "data" / "intermediate" / "tob" / "tavg"
DEF_QCU_DIR = REPO / "data" / "input" / "raw" / "tavg"
DEF_QCF_DIR = REPO / "data" / "output" / "qcf" / "tavg"
DEF_SOLUTIONS = REPO / "work" / "solutions"
DEF_INV = REPO / "data" / "input" / "station.inv"
DEF_TOB_BIN = REPO / "bin" / "TOBMain"
DEF_CACHE = REPO / "work" / "tob_basis"
DEF_SCRATCH = REPO / "work" / "tob_scratch"

MIN_SEG_VISIBLE = 18
MISMATCH_SAMPLES = 3

# ToboProvider: (sid, coord, knife_edges) -> {code: {(y, m): offset}}
ToboProvider = Callable[[str, Tuple[float, float], list], Dict[str, Dict]]


@dataclass
class VerifyResult:
    sid: str
    cls: str  # "tob" | "pha-only" | "verbatim"
    exact_pha: Optional[bool]  # None = not applicable
    exact_tob: Optional[bool]
    n_mismatch: int
    n_exempt: int
    resegmented: bool
    audits: List[str] = field(default_factory=list)
    samples: List[str] = field(default_factory=list)

    def tsv(self) -> str:
        def b(x):
            return "-" if x is None else str(x)

        return (
            f"{self.sid}\t{self.cls}\t{b(self.exact_pha)}\t{b(self.exact_tob)}"
            f"\t{self.n_mismatch}\t{self.n_exempt}\t{self.resegmented}"
            f"\t{';'.join(self.audits + self.samples)}"
        )


def _default_tobo_provider(
    tob_bin: Path, scratch: Path, cache: Path, qcu_dir: Path
) -> ToboProvider:
    def provider(sid, coord, knife_edges):
        from tob_basis import BasisRunner

        runner = BasisRunner(tob_bin, scratch, cache)
        basis = runner.get_bases(sid, qcu_dir / f"{sid}.raw.tavg", [coord])[0]
        # knife_edges entries are DOCUMENTATION ONLY (compiler-divergence
        # candidates recorded on plain readings; see residual_solver's
        # diagnostic-only patch policy).  Solutions are always solved and
        # emitted against the UNPATCHED basis, so the implied tobo must be
        # the raw basis too -- applying the candidate patches here would
        # make the verifier expect values the binary cannot produce.
        return {c: dict(m) for c, m in basis.code_offsets_by_ym.items()}

    return provider


def _month_le(a: Tuple[int, int], b: Tuple[int, int]) -> bool:
    return a <= b


def _regime_code_for(
    regimes: List[dict], ym: Tuple[int, int]
) -> Tuple[Optional[str], bool]:
    """(code, is_blend_month) for month ym; code None = before first regime."""
    code = None
    blend = False
    for r in regimes:
        by, bm, bd = r["begin"]
        if (by, bm) <= ym:
            code = r["code"]
            blend = (by, bm) == ym and (bd or 1) > 1
        else:
            break
    return code, blend


def _segment_for(segments: List[dict], ym: Tuple[int, int]) -> Optional[dict]:
    for s in segments:
        if tuple(s["begin"]) <= ym <= tuple(s["end"]):
            return s
    return None


def verify_station(
    sid: str,
    tob_dir: Path,
    qcu_dir: Path,
    qcf_dir: Path,
    solutions_dir: Path,
    inv: Dict[str, ghcn_io.Inv],
    tobo_provider: Optional[ToboProvider] = None,
) -> VerifyResult:
    tob_path = tob_dir / f"{sid}.tob.tavg"
    qcu_path = qcu_dir / f"{sid}.raw.tavg"
    qcf_path = qcf_dir / f"{sid}.qcf.tavg"
    sol_path = solutions_dir / f"{sid}.json"

    qcu = ghcn_io.read_station_data(qcu_path).values
    t_out = ghcn_io.read_station_data(tob_path).values
    qcf = ghcn_io.read_station_data(qcf_path).values if qcf_path.exists() else {}

    sol = None
    if sol_path.exists():
        with open(sol_path) as fh:
            sol = json.load(fh)

    audits: List[str] = []
    samples: List[str] = []
    n_mismatch = 0
    n_exempt = 0
    resegmented = False

    is_tob = sol is not None and sol.get("kind") == "tob" and sol.get("regimes")

    # ---- Verbatim check (no emitted .his -> TOBMain must copy) -------------
    if not is_tob:
        diffs = [
            ym for ym in sorted(set(qcu) | set(t_out)) if qcu.get(ym) != t_out.get(ym)
        ]
        exact_tob = not diffs
        for ym in diffs[:MISMATCH_SAMPLES]:
            samples.append(
                f"verbatim@{ym[0]}-{ym[1]:02d}:{qcu.get(ym)}->{t_out.get(ym)}"
            )
        n_mismatch += len(diffs)
        cls = "pha-only" if sol is not None else "verbatim"
    else:
        cls = "tob"
        exact_tob = True

    # ---- TOB side: t_out - qcu == implied tobo -----------------------------
    if is_tob:
        stored = sol.get("coord")
        if stored:
            coord = (stored[0], stored[1])
        else:
            rec = inv.get(sid)
            coord = (rec.lat, rec.lon) if rec else (0.0, 0.0)
        knife = [tuple(k) for k in sol.get("knife_edges", [])]
        offsets = tobo_provider(sid, coord, knife) if tobo_provider else {}
        regimes = sol["regimes"]
        dev_set = {tuple(d[0]) for d in sol.get("deviants", [])}
        tob_bad = 0
        for ym in sorted(set(qcu) & set(t_out)):
            code, blend = _regime_code_for(regimes, ym)
            if blend:
                n_exempt += 1
                continue
            expected = 0 if code is None else offsets.get(code, {}).get(tuple(ym))
            if expected is None:
                audits.append(f"no-basis@{ym[0]}-{ym[1]:02d}")
                continue
            if t_out[ym] - qcu[ym] != expected:
                tob_bad += 1
                if len(samples) < MISMATCH_SAMPLES:
                    samples.append(
                        f"tob@{ym[0]}-{ym[1]:02d}:{t_out[ym] - qcu[ym]}!={expected}"
                    )
        n_mismatch += tob_bad
        exact_tob = tob_bad == 0

    # ---- PHA side: qcf == pha_qcf(t_out, S) over solved segments -----------
    exact_pha: Optional[bool] = None
    if sol is not None:
        segments = sol.get("segments", [])
        dev_set = {tuple(d[0]) for d in sol.get("deviants", [])}
        pha_bad = 0
        checked = 0
        for seg in segments:
            lo_f, hi_f = seg["s_interval"]
            pairs = []
            for ym in sorted(set(t_out) & set(qcf)):
                if tuple(ym) in dev_set:
                    continue
                if not (tuple(seg["begin"]) <= ym <= tuple(seg["end"])):
                    continue
                pairs.append((ym, t_out[ym], qcf[ym]))
            if not pairs:
                continue

            def all_match(s_val) -> bool:
                return all(fp32.pha_qcf(t, s_val) == q for _, t, q in pairs)

            mid = fp32.f32((lo_f + hi_f) / 2.0)
            lo_ok, hi_ok = all_match(lo_f), all_match(hi_f)
            if lo_ok and hi_ok and all_match(mid):
                checked += len(pairs)
                continue
            # stale interval relative to t_out -> re-solve from scratch
            iv = fp32.solve_segment([(t, q) for _, t, q in pairs])
            if iv is not None:
                resegmented = True
                audits.append(f"resegmented@{seg['begin'][0]}-{seg['begin'][1]:02d}")
                checked += len(pairs)
                continue
            # count individual mismatching months against the best endpoint
            best = lo_f if lo_ok else hi_f
            for ym, t, q in pairs:
                if fp32.pha_qcf(t, best) != q:
                    pha_bad += 1
                    if len(samples) < MISMATCH_SAMPLES:
                        samples.append(
                            f"pha@{ym[0]}-{ym[1]:02d}:{fp32.pha_qcf(t, best)}!={q}"
                        )
        n_mismatch += pha_bad
        n_exempt += len(dev_set)
        exact_pha = pha_bad == 0

        # ---- audit: interior segment length ------------------------------
        for k, seg in enumerate(segments):
            if k == 0 or k == len(segments) - 1:
                continue
            by, bm = seg["begin"]
            ey, em = seg["end"]
            vis = seg.get("visible")
            if vis is None:
                vis = sum(1 for ym in qcf if (by, bm) <= ym <= (ey, em))
            if vis < MIN_SEG_VISIBLE:
                audits.append(f"short-segment@{by}-{bm:02d}")

    return VerifyResult(
        sid=sid,
        cls=cls,
        exact_pha=exact_pha,
        exact_tob=exact_tob,
        n_mismatch=n_mismatch,
        n_exempt=n_exempt,
        resegmented=resegmented,
        audits=audits,
        samples=samples,
    )


def _worker(job):
    sid, paths = job
    tob_dir, qcu_dir, qcf_dir, solutions, inv_path, tob_bin, scratch, cache = paths
    try:
        inv = _inv_cached(inv_path)
        provider = _default_tobo_provider(
            Path(tob_bin), Path(scratch), Path(cache), Path(qcu_dir)
        )
        res = verify_station(
            sid,
            Path(tob_dir),
            Path(qcu_dir),
            Path(qcf_dir),
            Path(solutions),
            inv,
            provider,
        )
        return res.tsv()
    except Exception as exc:  # pragma: no cover - per-station guard
        return f"{sid}\tERROR\t-\t-\t-\t-\t-\t{type(exc).__name__}: {exc}"


_INV = None


def _inv_cached(inv_path):
    global _INV
    if _INV is None:
        _INV = ghcn_io.read_inventory(Path(inv_path))
    return _INV


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--tob-dir", default=str(DEF_TOB_DIR))
    ap.add_argument("--qcu-dir", default=str(DEF_QCU_DIR))
    ap.add_argument("--qcf-dir", default=str(DEF_QCF_DIR))
    ap.add_argument("--solutions", default=str(DEF_SOLUTIONS))
    ap.add_argument("--inv", default=str(DEF_INV))
    ap.add_argument("--report", default=str(REPO / "work" / "verification_report.tsv"))
    ap.add_argument("--tob-bin", default=str(DEF_TOB_BIN))
    ap.add_argument("--jobs", type=int, default=1)
    ap.add_argument("--stations", default=None, help="comma-separated subset")
    args = ap.parse_args()

    tob_dir = Path(args.tob_dir)
    if args.stations:
        sids = [s.strip() for s in args.stations.split(",") if s.strip()]
    else:
        sids = sorted(p.name.split(".")[0] for p in tob_dir.glob("*.tob.tavg"))
    paths = (
        args.tob_dir,
        args.qcu_dir,
        args.qcf_dir,
        args.solutions,
        args.inv,
        args.tob_bin,
        str(DEF_SCRATCH),
        str(DEF_CACHE),
    )
    jobs = [(sid, paths) for sid in sids]
    lines: List[str] = []
    header = (
        "station\tclass\texact_pha\texact_tob\tn_mismatch\tn_exempt"
        "\tresegmented\taudits"
    )
    if args.jobs > 1:
        with mp.get_context("fork").Pool(args.jobs) as pool:
            for line in pool.imap_unordered(_worker, jobs, chunksize=8):
                lines.append(line)
    else:
        lines = [_worker(j) for j in jobs]

    lines.sort()
    report = Path(args.report)
    report.parent.mkdir(parents=True, exist_ok=True)
    with open(report, "w") as fh:
        fh.write(header + "\n" + "\n".join(lines) + "\n")

    n = len(lines)
    n_err = 0
    n_pha_ok = 0
    n_tob_ok = 0
    n_mis = 0
    for l in lines:
        cols = l.split("\t")
        if len(cols) < 7 or cols[1] == "ERROR":
            n_err += 1
            continue
        n_pha_ok += cols[2] == "True"
        n_tob_ok += cols[3] == "True"
        if cols[4].isdigit():
            n_mis += int(cols[4])
    print(
        f"# verified {n} stations: pha-exact {n_pha_ok}, tob-exact {n_tob_ok}, "
        f"total mismatched months {n_mis}, errors {n_err} -> {report}"
    )


if __name__ == "__main__":
    main()
