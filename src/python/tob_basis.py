#!/usr/bin/env python3
"""Fake-inventory batched TOBMain harness (basis-vector generation).

One TOBMain run per real station computes, up front, the full basis matrix
for every (candidate location x observation-time code) pair:

  * a temporary workspace holds a fake station.inv with one fake station per
    (coord, code) pair, all placed at the candidate coordinates;
  * the real station's raw data file is HARD-LINKED as every fake station's
    input, so the LTIX long-term means (and hence the bias table, including
    its float32 nint jitter) are exactly those of the real run;
  * each fake station gets a single-row .his asserting one code for all time.

Offset sign convention: offset(y, m) = tob_value - raw_value in integer
cents.  TOB application is value-dependent float32:
t = nint(f32(f32(f32(float(v))/100 - a_eff(m))*100)), so the offset is NOT
necessarily year-constant — at half-cent knife edges it differs by ±1
across years for the same (code, month).  A code with zero bias (24HR /
00HR, midnight) yields offsets identically 0.

Fake-id scheme (documented per contract): 11 chars, 'US' prefix so the
TOBMain CONUS gate admits them, deterministic:
    basis run:  "USB" + f"{coord_index:02d}" + f"{code_index:02d}" + "0000"
    blend run:  "USD" + f"{day:02d}" + "000000"
Each run lives in its own temp directory, so ids need only be unique within
a run (supports up to 100 coords x 100 codes).

NOTE: self-contained by design -- minimal fixed-width readers/writers are
duplicated here while ghcn_io.py is developed concurrently; consolidation
onto ghcn_io may follow.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import fp32

# Cache format version (the tob binary mtime in the key invalidates stale
# entries independently).
VERSION = 3

MISSING = -9999

# 27 basis codes: hourly 01..24 (24 == midnight == no adjustment) plus the
# seasonal specials (sunrise/sunset family).  TRID excluded (duplicates 00SS).
BASIS_CODES: List[str] = [f"{h:02d}HR" for h in range(1, 25)] + [
    "00RS",
    "00SR",
    "00SS",
]


# ---------------------------------------------------------------------------
# Minimal fixed-width I/O (see module docstring note about ghcn_io)
# ---------------------------------------------------------------------------


def read_station_values(path: Path) -> Dict[Tuple[int, int], int]:
    """Read a GHCN per-station file: (a11,a1,i4,12(i6,a3)).

    Returns {(year, month): cents} with missing months absent.
    """
    values: Dict[Tuple[int, int], int] = {}
    with open(path, "r") as fh:
        for line in fh:
            if len(line.rstrip("\n")) < 16:
                continue
            year = int(line[12:16])
            for m in range(12):
                start = 16 + m * 9
                raw = line[start : start + 6]
                if not raw.strip():
                    continue
                v = int(raw)
                if v == MISSING:
                    continue
                values[(year, m + 1)] = v
    return values


def first_data_year(values: Dict[Tuple[int, int], int]) -> int:
    return min(y for (y, _m) in values)


def _his_row(
    station_id: str,
    source: int,
    beg: Tuple[int, int, int],
    end: Tuple[int, int, int],
    obs_time: str,
) -> str:
    """Build a FORMAT 90 .his row with blank lat/lon/dist/elev/instr fields.

    Column map (1-based): 1 source, 2-13 station id (unread), 15-18/19-20/
    21-22 begin y/m/d, 24-27/28-29/30-31 end y/m/d, 78-81 obs_time.
    All other fields left blank (f3.0/i5 read blanks as 0; TOBMain ignores
    them entirely with tob.use-his-lat-lon=false).
    """
    by, bm, bd = beg
    ey, em, ed = end
    line = [" "] * 152
    line[0] = str(source)
    line[1 : 1 + min(len(station_id), 12)] = station_id[:12]
    line[14:18] = f"{by:04d}"
    line[18:20] = f"{bm:02d}"
    line[20:22] = f"{bd:02d}"
    line[23:27] = f"{ey:04d}"
    line[27:29] = f"{em:02d}"
    line[29:31] = f"{ed:02d}"
    line[77:81] = f"{obs_time:<4.4s}"
    return "".join(line)


def _write_properties(
    path: Path, work: Path, log_name: str, element: str = "tavg"
) -> None:
    """Write a TOBMain properties file pointing into the temp workspace.

    `element` selects the bias table.  TOBUtils computes a separate table for
    tmax, tmin and tavg (`compute_bias_tables`), and the tavg table is not the
    table for either of the others, so an element run against the wrong table
    gives offsets that match no real adjustment.
    """
    props = {
        "pha.logger.filename": str(work / (log_name + ".pha.log")),
        "pha.logger.level": "WARN",
        "pha.logger.print-to-stdout": "false",
        "pha.logger.append-datestamp": "false",
        "pha.logger.rollover-datestamp": "false",
        "pha.element": element,
        "pha.path.station-metadata": str(work / "station.inv"),
        "pha.path.station-history": str(work / "history") + "/",
        "tob.input-data-type": "raw",
        "tob.start-year": "1700",
        "tob.start-from-history": "true",
        "tob.backfill-if-first-nonblank": "false",
        "tob.pause-on-blank-after-nonblank": "false",
        "tob.use-his-lat-lon": "false",
        "tob.path.station-element-data-in": str(work / "raw" / element) + "/",
        "tob.path.station-element-data-out": str(work / "tob" / element) + "/",
        "tob.logger.filename": str(work / (log_name + ".log")),
        "tob.logger.level": "WARN",
        "tob.logger.print-to-stdout": "false",
        "tob.logger.append-datestamp": "false",
        "tob.logger.rollover-datestamp": "false",
    }
    with open(path, "w") as fh:
        for key, val in props.items():
            fh.write(f"{key} = {val}\n")


def _link_or_copy(src: Path, dst: Path) -> None:
    try:
        os.link(src, dst)
    except OSError:
        shutil.copyfile(src, dst)


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------


@dataclass
class Basis:
    """Per-(station, coordinate) basis: integer offsets per code.

    TOB application is VALUE-DEPENDENT float32:
    t = nint(f32(f32(f32(float(v))/100 − a_eff)·100)),
    so at half-cent knife edges the offset t − v can legitimately differ by
    ±1 across years for the same (code, month).  `code_offsets_by_ym` is the
    SOURCE OF TRUTH (exact per-(y, m) offsets from the matrix run against the
    hard-linked real data); `code_offsets[code][month]` is a majority-vote
    convenience.  Year-varying (code, month) cells are recorded in
    `anomalies` as (code, month, {offset: count}) — this is EXPECTED
    knife-edge value-dependent rounding (exact, deterministic), not error.

    `solve_a_eff` recovers the effective float32 adjustment a_eff(month,
    code) to sub-cent precision by interval-intersecting all (v, t) pairs of
    one calendar month across years — the fraction-recovery idea: ~70 years
    of one calendar month narrow a_eff far below 0.01 °C.
    """

    station_id: str
    coord: Tuple[float, float]
    code_offsets: Dict[str, Dict[int, int]] = field(default_factory=dict)
    code_offsets_by_ym: Dict[str, Dict[Tuple[int, int], int]] = field(
        default_factory=dict
    )
    anomalies: List[Tuple[str, int, Dict[int, int]]] = field(default_factory=list)
    # False when TOBMain never actually adjusted this station at this
    # coordinate (verbatim-copy path: mict < 5 short record, or candidate
    # coordinate outside the CONUS gate) -- the all-zero "basis" is
    # meaningless and must not be used for fitting.
    station_adjusted: bool = True

    def to_json(self) -> dict:
        return {
            "station_id": self.station_id,
            "coord": list(self.coord),
            "station_adjusted": self.station_adjusted,
            "code_offsets": {
                c: {str(m): o for m, o in mm.items()}
                for c, mm in self.code_offsets.items()
            },
            "code_offsets_by_ym": {
                c: {f"{y}-{m:02d}": o for (y, m), o in mm.items()}
                for c, mm in self.code_offsets_by_ym.items()
            },
            "anomalies": [
                [c, m, {str(k): v for k, v in d.items()}]
                for (c, m, d) in self.anomalies
            ],
        }

    @staticmethod
    def from_json(obj: dict) -> "Basis":
        b = Basis(
            station_id=obj["station_id"], coord=(obj["coord"][0], obj["coord"][1])
        )
        b.station_adjusted = obj.get("station_adjusted", True)
        b.code_offsets = {
            c: {int(m): o for m, o in mm.items()}
            for c, mm in obj["code_offsets"].items()
        }
        for c, mm in obj["code_offsets_by_ym"].items():
            out: Dict[Tuple[int, int], int] = {}
            for key, o in mm.items():
                y, m = key.split("-")
                out[(int(y), int(m))] = o
            b.code_offsets_by_ym[c] = out
        b.anomalies = [
            (c, m, {int(k): v for k, v in d.items()}) for c, m, d in obj["anomalies"]
        ]
        return b

    def solve_a_eff(
        self,
        code: str,
        month: int,
        raw_values: Dict[Tuple[int, int], int],
    ):
        """Exact float32 interval for a_eff(month, code), or None.

        Collects (v, t) = (raw, raw + offset) for every year of `month` with
        data and solves t = nint(f32(f32(f32(float(v))/100 − a_eff)·100))
        via fp32.solve_segment (identical functional form to the PHA chain,
        a_eff in place of S).  Returns None when there are no pairs, or when
        the pairs are jointly inconsistent (should not happen for offsets
        measured from a real run).
        """
        by_ym = self.code_offsets_by_ym.get(code, {})
        pairs = []
        for (y, m), off in by_ym.items():
            if m != month:
                continue
            v = raw_values.get((y, m))
            if v is None:
                continue
            pairs.append((v, v + off))
        if not pairs:
            return None
        return fp32.solve_segment(pairs)

    def solve_all_a_eff(
        self, raw_values: Dict[Tuple[int, int], int]
    ) -> Dict[str, Dict[int, "fp32.Interval"]]:
        """{code: {month: Interval}} over all cells with data (None skipped)."""
        out: Dict[str, Dict[int, fp32.Interval]] = {}
        for code in self.code_offsets_by_ym:
            for month in range(1, 13):
                iv = self.solve_a_eff(code, month, raw_values)
                if iv is not None:
                    out.setdefault(code, {})[month] = iv
        return out


def _station_adjusted(offsets_by_code: Dict[str, Dict[Tuple[int, int], int]]) -> bool:
    """True iff TOBMain actually computed adjustments for this station/coord.

    Detection choice (documented per review finding G): TOBMain
    verbatim-copies the input when the station's max monthly count mict < 5
    (TOBMain.f95:252) or the coordinate fails the CONUS gate -- then EVERY
    code yields all-zero offsets.  Conversely, a genuinely adjusted CONUS
    station never has uniformly zero offsets across all hourly codes (e.g.
    07HR/17HR are essentially never zero-bias year-round).  The all-codes-
    zero condition is therefore decisive, is already computed, and -- unlike
    a byte-identity check on the output file -- does not depend on
    distinguishing TOBMain's copy path from its rewrite path.
    """
    return any(off != 0 for by_ym in offsets_by_code.values() for off in by_ym.values())


def _consensus(
    by_ym: Dict[Tuple[int, int], int],
) -> Tuple[Dict[int, int], List[Tuple[int, Dict[int, int]]]]:
    """Collapse per-(y,m) offsets to per-month; report disagreements."""
    per_month: Dict[int, Dict[int, int]] = {}
    for (y, m), off in by_ym.items():
        per_month.setdefault(m, {}).setdefault(off, 0)
        per_month[m][off] += 1
    consensus: Dict[int, int] = {}
    anomalies: List[Tuple[int, Dict[int, int]]] = []
    for m, counts in per_month.items():
        best = max(counts.items(), key=lambda kv: kv[1])[0]
        consensus[m] = best
        if len(counts) > 1:
            anomalies.append((m, dict(counts)))
    return consensus, anomalies


# ---------------------------------------------------------------------------
# The harness
# ---------------------------------------------------------------------------


class BasisRunner:
    def __init__(
        self,
        tob_bin: Path,
        scratch_root: Path,
        cache_root: Path,
        element: str = "tavg",
    ):
        self.tob_bin = Path(tob_bin)
        self.scratch_root = Path(scratch_root)
        self.cache_root = Path(cache_root)
        self.element = element
        self.scratch_root.mkdir(parents=True, exist_ok=True)
        self.cache_root.mkdir(parents=True, exist_ok=True)

    # -- cache -------------------------------------------------------------
    def _cache_key(self, raw_path: Path, coords, transform) -> str:
        st = os.stat(raw_path)
        tb = os.stat(self.tob_bin)
        blob = json.dumps(
            {
                "v": VERSION,
                # The element picks the bias table, so two runs over one raw
                # file under different elements are different results.
                "element": self.element,
                "coords": [[round(lat, 6), round(lon, 6)] for lat, lon in coords],
                "transform": repr(transform),
                "raw": [st.st_size, int(st.st_mtime)],
                "tob_bin": int(tb.st_mtime),
            },
            sort_keys=True,
        )
        return hashlib.sha1(blob.encode()).hexdigest()

    # -- workspace construction --------------------------------------------
    def _materialize_raw(self, raw_path: Path, transform) -> Path:
        """Return the input file to hard-link (optionally transformed)."""
        if transform is None:
            return raw_path
        # transform: {"start_year": int|None, "end_year": int|None,
        #             "drop_qc": bool} -- LTIX window variants.
        start = transform.get("start_year")
        end = transform.get("end_year")
        drop_qc = transform.get("drop_qc", False)
        out_lines = []
        with open(raw_path) as fh:
            for line in fh:
                line = line.rstrip("\n")
                if len(line) < 16:
                    continue
                year = int(line[12:16])
                if start is not None and year < start:
                    continue
                if end is not None and year > end:
                    continue
                if drop_qc:
                    chars = list(line)
                    for m in range(12):
                        qc_pos = 16 + m * 9 + 7
                        if qc_pos < len(chars) and chars[qc_pos] != " ":
                            chars[16 + m * 9 : 16 + m * 9 + 6] = f"{MISSING:6d}"
                    line = "".join(chars)
                out_lines.append(line)
        tf = tempfile.NamedTemporaryFile(
            mode="w",
            dir=self.scratch_root,
            suffix=f".raw.{self.element}",
            delete=False,
        )
        tf.write("\n".join(out_lines) + "\n")
        tf.close()
        return Path(tf.name)

    def _run_workspace(
        self,
        station_id: str,
        raw_path: Path,
        fakes: List[Tuple[str, float, float, List[str]]],
        transform,
    ) -> Dict[str, Dict[Tuple[int, int], int]]:
        """Build workspace, run TOBMain once, return per-fake-id offsets.

        fakes: list of (fake_id, lat, lon, his_lines).
        """
        raw_src = self._materialize_raw(raw_path, transform)
        raw_values = read_station_values(raw_src)
        if not raw_values:
            raise RuntimeError(f"{station_id}: no data in {raw_src}")

        work = Path(
            tempfile.mkdtemp(prefix=f"tob_{station_id}_", dir=self.scratch_root)
        )
        ok = False
        try:
            (work / "raw" / self.element).mkdir(parents=True)
            (work / "tob" / self.element).mkdir(parents=True)
            (work / "history").mkdir()
            inv_lines = []
            for fake_id, lat, lon, his_lines in fakes:
                if len(fake_id) != 11 or not fake_id.startswith("US"):
                    raise ValueError(f"bad fake id {fake_id!r}")
                inv_lines.append(
                    f"{fake_id:<11s}{lat:9.4f}{lon:10.4f}{100.0:7.1f} FAKE"
                )
                # A fake station must copy the (possibly transformed) input.
                _link_or_copy(
                    raw_src,
                    work / "raw" / self.element / f"{fake_id}.raw.{self.element}",
                )
                with open(work / "history" / f"{fake_id}.his", "w") as fh:
                    fh.write("\n".join(his_lines) + "\n")
            (work / "station.inv").write_text("\n".join(inv_lines) + "\n")
            props = work / "tob.properties"
            _write_properties(props, work, "tob", self.element)

            res = subprocess.run(
                [str(self.tob_bin), "-p", str(props)], capture_output=True, text=True
            )
            if res.returncode != 0:
                raise RuntimeError(
                    f"TOBMain failed rc={res.returncode}\n"
                    f"stdout: {res.stdout[-2000:]}\nstderr: {res.stderr[-2000:]}"
                )

            out: Dict[str, Dict[Tuple[int, int], int]] = {}
            for fake_id, _lat, _lon, _his in fakes:
                tob_file = (
                    work / "tob" / self.element / f"{fake_id}.tob.{self.element}"
                )
                if not tob_file.exists():
                    raise RuntimeError(
                        f"TOBMain produced no output for {fake_id} "
                        f"(station {station_id}); log: "
                        f"{(work / 'tob.log')}"
                    )
                tob_values = read_station_values(tob_file)
                offsets = {}
                for ym, raw_v in raw_values.items():
                    if ym in tob_values:
                        offsets[ym] = tob_values[ym] - raw_v
                out[fake_id] = offsets
            ok = True
            return out
        finally:
            if ok:
                shutil.rmtree(work, ignore_errors=True)
                if raw_src != raw_path:
                    raw_src.unlink(missing_ok=True)
            # on failure: keep workspace for debugging

    # -- public API ---------------------------------------------------------
    def get_bases_cached(
        self,
        station_id: str,
        raw_path: Path,
        coords: List[Tuple[float, float]],
        transform: Optional[dict] = None,
    ) -> Optional[List[Basis]]:
        """Return cached bases only; None on a cache miss (never runs TOBMain).

        Used by consolidate-only tooling that must not solve."""
        raw_path = Path(raw_path)
        key = self._cache_key(raw_path, coords, transform)
        cache_file = self.cache_root / station_id / f"{key}.json"
        if not cache_file.exists():
            return None
        with open(cache_file) as fh:
            payload = json.load(fh)
        return [Basis.from_json(o) for o in payload["bases"]]

    def get_bases(
        self,
        station_id: str,
        raw_path: Path,
        coords: List[Tuple[float, float]],
        transform: Optional[dict] = None,
    ) -> List[Basis]:
        """Basis matrices for all (coord x BASIS_CODES), one TOBMain run.

        Returns a list parallel to `coords`.
        """
        raw_path = Path(raw_path)
        key = self._cache_key(raw_path, coords, transform)
        cache_file = self.cache_root / station_id / f"{key}.json"
        if cache_file.exists():
            with open(cache_file) as fh:
                payload = json.load(fh)
            return [Basis.from_json(o) for o in payload["bases"]]

        raw_values = read_station_values(raw_path)
        fdy = first_data_year(raw_values)

        fakes = []
        for c, (lat, lon) in enumerate(coords):
            for k, code in enumerate(BASIS_CODES):
                fake_id = f"USB{c:02d}{k:02d}0000"
                his = _his_row(fake_id, 0, (fdy, 1, 1), (9999, 12, 31), code)
                fakes.append((fake_id, lat, lon, [his]))

        by_fake = self._run_workspace(station_id, raw_path, fakes, transform)

        bases = []
        for c, (lat, lon) in enumerate(coords):
            basis = Basis(station_id=station_id, coord=(lat, lon))
            for k, code in enumerate(BASIS_CODES):
                fake_id = f"USB{c:02d}{k:02d}0000"
                by_ym = by_fake[fake_id]
                basis.code_offsets_by_ym[code] = by_ym
                consensus, monthly_anoms = _consensus(by_ym)
                basis.code_offsets[code] = consensus
                for m, counts in monthly_anoms:
                    basis.anomalies.append((code, m, counts))
            basis.station_adjusted = _station_adjusted(basis.code_offsets_by_ym)
            bases.append(basis)

        cache_file.parent.mkdir(parents=True, exist_ok=True)
        tmp = cache_file.with_suffix(".tmp")
        with open(tmp, "w") as fh:
            json.dump({"version": VERSION, "bases": [b.to_json() for b in bases]}, fh)
        tmp.replace(cache_file)
        return bases

    def blend_offsets(
        self,
        station_id: str,
        raw_path: Path,
        coord: Tuple[float, float],
        switch: Tuple[int, int],
        left_code: str,
        right_code: str,
        days,
    ) -> Dict[int, Dict[Tuple[int, int], int]]:
        """Offsets for a mid-month code switch, per candidate day.

        `switch` is the (year, month) of the changeover; for each candidate
        day d the fake .his has row1 = left_code from the record start and
        row2 = right_code beginning (year, month, d).  Returns
        {day: {(y, m): offset}} over the full record, so callers see pure
        left offsets before the switch, the day-weighted blend in the switch
        month, and pure right offsets after.
        """
        raw_path = Path(raw_path)
        raw_values = read_station_values(raw_path)
        fdy = first_data_year(raw_values)
        year, month = switch
        lat, lon = coord

        fakes = []
        for d in days:
            fake_id = f"USD{d:02d}000000"
            rows = [
                _his_row(
                    fake_id, 0, (fdy, 1, 1), _day_before(year, month, d), left_code
                ),
                _his_row(fake_id, 0, (year, month, d), (9999, 12, 31), right_code),
            ]
            fakes.append((fake_id, lat, lon, rows))

        by_fake = self._run_workspace(station_id, raw_path, fakes, None)
        return {d: by_fake[f"USD{d:02d}000000"] for d in days}


def _day_before(year: int, month: int, day: int) -> Tuple[int, int, int]:
    if day > 1:
        return (year, month, day - 1)
    if month > 1:
        return (year, month - 1, _days_in_month(year, month - 1))
    return (year - 1, 12, 31)


def _days_in_month(year: int, month: int) -> int:
    if month == 2:
        leap = year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)
        return 29 if leap else 28
    return [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][month - 1]


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def _read_inventory_coord(inv_path: Path, station_id: str):
    with open(inv_path) as fh:
        for line in fh:
            if line[:11].strip() == station_id:
                return float(line[12:20]), float(line[21:30])
    raise SystemExit(f"{station_id} not in {inv_path}")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--station", required=True)
    ap.add_argument(
        "--coord",
        action="append",
        default=[],
        help="lat,lon (repeatable; default: inventory coord)",
    )
    ap.add_argument("--raw", default=None)
    ap.add_argument(
        "--element",
        default="tavg",
        choices=("tavg", "tmax", "tmin"),
        help="Bias table to use; each element has its own",
    )
    ap.add_argument("--inv", default="data/input/station.inv")
    ap.add_argument("--tob-bin", default="bin/TOBMain")
    ap.add_argument("--scratch", default=None)
    ap.add_argument("--cache", default="work/tob_basis")
    args = ap.parse_args()

    raw = Path(
        args.raw
        or f"data/input/raw/{args.element}/{args.station}.raw.{args.element}"
    )
    coords = []
    for c in args.coord:
        lat, lon = c.split(",")
        coords.append((float(lat), float(lon)))
    if not coords:
        coords = [_read_inventory_coord(Path(args.inv), args.station)]

    scratch = Path(args.scratch or tempfile.gettempdir()) / "tob_basis_cli"
    runner = BasisRunner(
        Path(args.tob_bin), scratch, Path(args.cache), args.element
    )
    t0 = time.time()
    bases = runner.get_bases(args.station, raw, coords)
    dt = time.time() - t0

    for basis in bases:
        print(
            f"# {basis.station_id} @ {basis.coord[0]:.4f},"
            f"{basis.coord[1]:.4f}  ({dt:.1f}s)"
        )
        header = "code | " + " ".join(f"{m:>4d}" for m in range(1, 13))
        print(header)
        print("-" * len(header))
        for code in BASIS_CODES:
            row = basis.code_offsets.get(code, {})
            cells = " ".join(f"{row.get(m, 0):>4d}" for m in range(1, 13))
            print(f"{code} | {cells}")
        if basis.anomalies:
            print(f"! anomalies: {basis.anomalies}")


if __name__ == "__main__":
    main()
