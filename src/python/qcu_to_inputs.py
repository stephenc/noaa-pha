#!/usr/bin/env python3
"""Convert GHCN-M QCU tar.gz into PHA input directory layout.

Fixed-width parsing only. No whitespace parsing.
"""
from __future__ import annotations

import argparse
import tarfile
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple


INV_ID_SLICE = (0, 11)
INV_LAT_SLICE = (12, 20)
INV_LON_SLICE = (21, 30)
INV_ELEV_SLICE = (31, 37)

DAT_ID_SLICE = (0, 11)
DAT_YEAR_SLICE = (11, 15)
DAT_ELEM_SLICE = (15, 19)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Convert QCU tar.gz to PHA inputs")
    parser.add_argument("--qcu-tar", required=True, help="Path to ghcnm.*.qcu.tar.gz")
    parser.add_argument("--base", required=True, help="Base output directory")
    parser.add_argument("--version", default="v4", help="Version string (max 4 chars)")
    parser.add_argument("--begin-year", type=int, default=1700, help="pha.begin-year")
    parser.add_argument("--data-type", help="Optional 1-char data type code between station id and year")
    parser.add_argument("--dry-run", action="store_true", help="Do not write files")
    return parser.parse_args()


def clamp_version(version: str) -> str:
    return version[:4]


def ensure_dirs(base: Path, dry_run: bool) -> Dict[str, Path]:
    paths = {
        "input_raw_tavg": base / "input" / "raw" / "tavg",
        "input_tob_tavg": base / "input" / "tob" / "tavg",
        "input_history": base / "input" / "history",
        "output_adj": base / "output" / "adj" / "tavg",
    }
    if not dry_run:
        for path in paths.values():
            path.mkdir(parents=True, exist_ok=True)
    return paths


def find_members(tar: tarfile.TarFile) -> Tuple[str, str]:
    dat_candidates: List[str] = []
    inv_candidates: List[str] = []
    for member in tar.getmembers():
        name = member.name
        if name.lower().endswith(".dat") and "tavg" in name.lower():
            dat_candidates.append(name)
        if name.lower().endswith(".inv") and "tavg" in name.lower():
            inv_candidates.append(name)
    if not dat_candidates or not inv_candidates:
        raise RuntimeError(
            "Unable to find required tavg .dat and .inv in archive. "
            f"dat={dat_candidates} inv={inv_candidates}"
        )
    return dat_candidates[0], inv_candidates[0]


def write_inventory(tar: tarfile.TarFile, inv_member: str, out_path: Path, dry_run: bool) -> int:
    count = 0
    if dry_run:
        with tar.extractfile(inv_member) as fh:
            if fh is None:
                return 0
            for _ in fh:
                count += 1
        return count

    with tar.extractfile(inv_member) as fh:
        if fh is None:
            return 0
        with out_path.open("w", encoding="utf-8") as out:
            for raw in fh:
                try:
                    line = raw.decode("utf-8")
                except UnicodeDecodeError:
                    line = raw.decode("latin-1", errors="replace")
                if len(line) < 68:
                    continue
                out.write(line.rstrip("\n") + "\n")
                count += 1
    return count


def format_station_line(station_id: str, year: int, values: List[int], flags: List[str], data_type: str) -> str:
    parts = [f"{station_id:11s}", data_type, f"{year:4d}"]
    for i in range(12):
        parts.append(f"{values[i]:6d}")
        parts.append(f"{flags[i]:3s}")
    return "".join(parts)


def parse_value(value_raw: str) -> int:
    value_raw = value_raw.strip()
    if not value_raw:
        return -9999
    try:
        return int(value_raw)
    except ValueError:
        return -9999


def parse_flags(dm: str, qc: str, ds: str) -> str:
    return f"{dm}{qc}{ds}"


def write_station_data(
    tar: tarfile.TarFile,
    dat_member: str,
    out_dir: Path,
    data_type: str,
    dry_run: bool,
) -> Tuple[int, int]:
    line_count = 0
    station_files: Dict[str, Optional[Path]] = {}

    with tar.extractfile(dat_member) as fh:
        if fh is None:
            return 0, 0
        for raw in fh:
            line_count += 1
            try:
                line = raw.decode("utf-8")
            except UnicodeDecodeError:
                line = raw.decode("latin-1", errors="replace")
            if len(line) < 115:
                continue
            station_id = line[DAT_ID_SLICE[0]:DAT_ID_SLICE[1]]
            year_raw = line[DAT_YEAR_SLICE[0]:DAT_YEAR_SLICE[1]]
            elem = line[DAT_ELEM_SLICE[0]:DAT_ELEM_SLICE[1]]
            if elem != "TAVG":
                continue
            try:
                year = int(year_raw)
            except ValueError:
                continue

            values: List[int] = []
            flags: List[str] = []
            base = 19
            for month in range(12):
                offset = base + month * 8
                value_raw = line[offset:offset + 5]
                dm = line[offset + 5:offset + 6]
                qc = line[offset + 6:offset + 7]
                ds = line[offset + 7:offset + 8]
                values.append(parse_value(value_raw))
                flags.append(parse_flags(dm, qc, ds))

            out_line = format_station_line(station_id, year, values, flags, data_type)

            if dry_run:
                station_files[station_id] = None
                continue

            out_path = out_dir / f"{station_id}.raw.tavg"
            if station_id not in station_files:
                station_files[station_id] = out_path
            with out_path.open("a", encoding="utf-8") as out:
                out.write(out_line + "\n")

    return line_count, len(station_files)


def write_properties(base: Path, begin_year: int, version: str, dry_run: bool) -> None:
    version = clamp_version(version)
    common = [
        "pha.logger.filename = " + str(base / "output" / "pha.log"),
        "pha.logger.level = INFO",
        "pha.logger.print-to-stdout = false",
        "pha.logger.append-datestamp = true",
        "pha.logger.rollover-datestamp = false",
        "",
        f"pha.begin-year = {begin_year}",
        "pha.element = tavg",
        f"pha.version = {version}",
        "",
        "pha.path.station-metadata = " + str(base / "input" / "station.inv"),
        "pha.path.station-history = " + str(base / "input" / "history") + "/",
        "pha.path.station-element-data-in = " + str(base / "input" / "{pha.input-data-type}" / "tavg") + "/",
        "pha.path.neighbors.station-element-data-in = " + str(base / "input" / "{pha.neighbors.input-data-type}" / "tavg") + "/",
        "pha.path.station-element-data-out = " + str(base / "output" / "adj" / "tavg") + "/",
        "",
        "pha.path.neighbors.distance = " + str(base / "output" / "neighbor-distance.txt"),
        "pha.path.neighbors.correlation = " + str(base / "output" / "neighbor-correlation.txt"),
        "pha.path.neighbors.correlation-in = " + str(base / "output" / "neighbor-correlation.txt"),
        "",
        "pha.neighbors.distance-neighbor-limit = 40",
        "pha.neighbors.final-neighbor-limit = 20",
        "pha.neighbors.method = first-diffs",
        "pha.neighbors.min-station-coverage = 7",
        "pha.neighbors.min-coefficient = 0.1",
        "pha.snht-threshold = 5",
        "pha.bic-penalty = bic",
        "pha.amploc-percent = 92",
        "pha.confirm = 2",
        "pha.adjust.min-length = 18",
        "pha.adjust.min-neighbors = 2",
        "pha.adjust.remove-outliers = true",
        "pha.adjust.window = 0",
        "pha.adjust.filter-method = conf",
        "pha.adjust.est-method = med",
        "pha.remove-insignificant = true",
        "pha.use-history-files = 1",
        "pha.do-run-main = true",
        "pha.do-run-neighbors = true",
    ]

    def write_prop(path: Path, input_type: str, neighbors_type: str, include_tob: bool) -> None:
        lines = common + [
            "",
            f"pha.input-data-type = {input_type}",
            f"pha.neighbors.input-data-type = {neighbors_type}",
        ]
        if include_tob:
            lines.extend([
                "tob.start-year = 1890",
                "tob.start-from-history = true",
                "tob.path.station-element-data-in = " + str(base / "input" / "raw" / "tavg") + "/",
                "tob.path.station-element-data-out = " + str(base / "input" / "tob" / "tavg") + "/",
                "tob.logger.filename = " + str(base / "output" / "tob.log"),
                "tob.logger.level = INFO",
                "tob.logger.print-to-stdout = false",
                "tob.logger.append-datestamp = true",
                "tob.logger.rollover-datestamp = false",
            ])
        if dry_run:
            return
        with path.open("w", encoding="utf-8") as fh:
            fh.write("\n".join(lines) + "\n")

    write_prop(base / "raw.properties", "raw", "raw", include_tob=False)
    write_prop(base / "tob.properties", "tob", "tob", include_tob=True)


def main() -> int:
    args = parse_args()
    base = Path(args.base)

    paths = ensure_dirs(base, args.dry_run)

    with tarfile.open(args.qcu_tar, "r:gz") as tar:
        dat_member, inv_member = find_members(tar)
        print(f"Using data member: {dat_member}")
        print(f"Using inv member:  {inv_member}")

        inv_out = base / "input" / "station.inv"
        inv_count = write_inventory(tar, inv_member, inv_out, args.dry_run)

        data_type = args.data_type if args.data_type is not None else " "
        if len(data_type) != 1:
            raise RuntimeError("--data-type must be exactly 1 character if provided")

        data_lines, station_count = write_station_data(
            tar,
            dat_member,
            paths["input_raw_tavg"],
            data_type,
            args.dry_run,
        )

    write_properties(base, args.begin_year, args.version, args.dry_run)

    print("Summary")
    print(f"  Inventory lines written: {inv_count}")
    print(f"  Data lines read:         {data_lines}")
    print(f"  Stations written:        {station_count}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
