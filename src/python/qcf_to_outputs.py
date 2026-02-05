#!/usr/bin/env python3
"""Convert GHCN-M QCF tar.gz into per-station output files for comparison.

Fixed-width parsing only. No whitespace parsing. Inventory is ignored.
"""
from __future__ import annotations

import argparse
import tarfile
from pathlib import Path
from typing import Dict, List, Set, Tuple

DAT_ID_SLICE = (0, 11)
DAT_YEAR_SLICE = (11, 15)
DAT_ELEM_SLICE = (15, 19)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Convert QCF tar.gz to output files")
    parser.add_argument("--qcf-tar", required=True, help="Path to ghcnm.*.qcf.tar.gz")
    parser.add_argument("--base", required=True, help="Base output directory")
    parser.add_argument("--data-type", help="Optional 1-char data type code between station id and year")
    parser.add_argument("--dry-run", action="store_true", help="Do not write files")
    return parser.parse_args()


def find_dat_member(tar: tarfile.TarFile) -> str:
    dat_candidates: List[str] = []
    for member in tar.getmembers():
        name = member.name
        if name.lower().endswith(".dat") and "tavg" in name.lower():
            dat_candidates.append(name)
    if not dat_candidates:
        raise RuntimeError("Unable to find required tavg .dat in archive")
    return dat_candidates[0]


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
    station_files: Dict[str, Path] = {}
    seen: Set[str] = set()

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
                station_files[station_id] = out_dir / f"{station_id}.qcf.tavg"
                continue

            out_path = out_dir / f"{station_id}.qcf.tavg"
            if station_id not in station_files:
                station_files[station_id] = out_path
            mode = "w" if station_id not in seen else "a"
            seen.add(station_id)
            with out_path.open(mode, encoding="utf-8") as out:
                out.write(out_line + "\n")

    return line_count, len(station_files)


def main() -> int:
    args = parse_args()
    base = Path(args.base)
    out_dir = base / "output" / "qcf" / "tavg"
    if not args.dry_run:
        out_dir.mkdir(parents=True, exist_ok=True)

    data_type = args.data_type if args.data_type is not None else " "
    if len(data_type) != 1:
        raise RuntimeError("--data-type must be exactly 1 character if provided")

    with tarfile.open(args.qcf_tar, "r:gz") as tar:
        dat_member = find_dat_member(tar)
        print(f"Using data member: {dat_member}")

        data_lines, station_count = write_station_data(
            tar,
            dat_member,
            out_dir,
            data_type,
            args.dry_run,
        )

    print("Summary")
    print(f"  Data lines read:   {data_lines}")
    print(f"  Stations written:  {station_count}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
