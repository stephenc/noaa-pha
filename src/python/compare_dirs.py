#!/usr/bin/env python3
"""Compare two directories of per-station files and summarize differences.

Assumes filename first segment (before '.') is the station id.
Only compares stations present in both directories.
"""
from __future__ import annotations

import argparse
import math
import mmap
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Compare per-station files across two directories")
    parser.add_argument("left", help="Left directory")
    parser.add_argument("right", help="Right directory")
    parser.add_argument("--header", action="store_true", help="Include CSV header row")
    parser.add_argument("--no-mmap", action="store_true", help="Disable memory-mapped file reads")
    return parser.parse_args()


def list_station_files(directory: Path) -> Dict[str, Path]:
    mapping: Dict[str, Path] = {}
    for entry in directory.iterdir():
        if not entry.is_file():
            continue
        name = entry.name
        station_id = name.split(".", 1)[0]
        if station_id:
            mapping[station_id] = entry
    return mapping


def iter_station_lines(path: Path, use_mmap: bool) -> Iterable[bytes]:
    with path.open("rb") as fh:
        if use_mmap:
            try:
                with mmap.mmap(fh.fileno(), 0, access=mmap.ACCESS_READ) as mm:
                    for line in iter(mm.readline, b""):
                        yield line
                return
            except ValueError:
                pass
        for line in fh:
            yield line


def parse_line(line: bytes) -> Optional[Tuple[int, List[int], List[str]]]:
    if len(line) < 16 + 12 * 9:
        return None
    try:
        year = int(line[12:16])
    except ValueError:
        return None
    values: List[int] = []
    qcflags: List[str] = []
    base = 16
    for month in range(12):
        offset = base + month * 9
        flag = line[offset + 6:offset + 9]
        value_raw = line[offset:offset + 6].strip()
        try:
            value = int(value_raw) if value_raw else -9999
        except ValueError:
            value = -9999
        qc = chr(flag[1]) if len(flag) >= 2 else ""
        values.append(value)
        qcflags.append(qc)
    return year, values, qcflags


def compare_station_files(
    left_path: Path,
    right_path: Path,
    use_mmap: bool,
    stats: Tuple[float, float, int],
    bin_stats: Tuple[Dict[int, float], Dict[int, float], Dict[int, int]],
    max_year: Optional[int],
) -> Tuple[Tuple[float, float, int], Tuple[Dict[int, float], Dict[int, float], Dict[int, int]], Optional[int]]:
    total_sum, total_sq, total_count = stats
    bin_sums, bin_sqs, bin_counts = bin_stats

    left_iter = iter_station_lines(left_path, use_mmap)
    right_iter = iter_station_lines(right_path, use_mmap)

    left_line = next(left_iter, None)
    right_line = next(right_iter, None)
    left_parsed = parse_line(left_line) if left_line else None
    right_parsed = parse_line(right_line) if right_line else None

    while left_parsed is not None and right_parsed is not None:
        left_year, left_vals, left_qc = left_parsed
        right_year, right_vals, right_qc = right_parsed

        if left_year == right_year:
            year = left_year
            for m in range(12):
                if left_qc[m] != " " or right_qc[m] != " ":
                    continue
                if left_vals[m] == -9999 or right_vals[m] == -9999:
                    continue
                diff = float(left_vals[m] - right_vals[m]) / 100.0
                total_sum, total_sq, total_count = update_stats(total_sum, total_sq, total_count, diff)
                bin_index = (year - 1700) // 30
                bin_sums[bin_index], bin_sqs[bin_index], bin_counts[bin_index] = update_stats(
                    bin_sums.get(bin_index, 0.0),
                    bin_sqs.get(bin_index, 0.0),
                    bin_counts.get(bin_index, 0),
                    diff,
                )
                if max_year is None or year > max_year:
                    max_year = year

            left_line = next(left_iter, None)
            right_line = next(right_iter, None)
            left_parsed = parse_line(left_line) if left_line else None
            right_parsed = parse_line(right_line) if right_line else None
        elif left_year < right_year:
            left_line = next(left_iter, None)
            left_parsed = parse_line(left_line) if left_line else None
        else:
            right_line = next(right_iter, None)
            right_parsed = parse_line(right_line) if right_line else None

    return (total_sum, total_sq, total_count), (bin_sums, bin_sqs, bin_counts), max_year


def update_stats(sum_diff: float, sum_sq: float, count: int, diff: float) -> Tuple[float, float, int]:
    return sum_diff + diff, sum_sq + diff * diff, count + 1


def main() -> int:
    args = parse_args()
    left_dir = Path(args.left)
    right_dir = Path(args.right)

    left_map = list_station_files(left_dir)
    right_map = list_station_files(right_dir)

    matched_ids = sorted(set(left_map.keys()) & set(right_map.keys()))
    left_unmatched = len(left_map) - len(matched_ids)
    right_unmatched = len(right_map) - len(matched_ids)

    total_sum = 0.0
    total_sq = 0.0
    total_count = 0

    bin_sums: Dict[int, float] = {}
    bin_sqs: Dict[int, float] = {}
    bin_counts: Dict[int, int] = {}

    max_year: Optional[int] = None

    for station_id in matched_ids:
        stats = (total_sum, total_sq, total_count)
        bin_stats = (bin_sums, bin_sqs, bin_counts)
        stats, bin_stats, max_year = compare_station_files(
            left_map[station_id],
            right_map[station_id],
            use_mmap=not args.no_mmap,
            stats=stats,
            bin_stats=bin_stats,
            max_year=max_year,
        )
        total_sum, total_sq, total_count = stats
        bin_sums, bin_sqs, bin_counts = bin_stats

    def mean_and_rms(sum_diff: float, sum_sq: float, count: int) -> Tuple[Optional[float], Optional[float]]:
        if count == 0:
            return None, None
        mean = sum_diff / count
        rms = math.sqrt(sum_sq / count)
        return mean, rms

    all_mean, all_rms = mean_and_rms(total_sum, total_sq, total_count)

    # Determine bins up to the present bin based on max_year found
    bin_headers: List[str] = []
    bin_values: List[str] = []
    if max_year is not None:
        last_bin = (max_year - 1700) // 30
        for bin_index in range(0, last_bin + 1):
            start_year = 1700 + bin_index * 30
            end_year = start_year + 29
            header_mean = f"{start_year}_{end_year}_mean"
            header_rms = f"{start_year}_{end_year}_rms"
            bin_headers.extend([header_mean, header_rms])
            mean, rms = mean_and_rms(
                bin_sums.get(bin_index, 0.0),
                bin_sqs.get(bin_index, 0.0),
                bin_counts.get(bin_index, 0),
            )
            bin_values.extend([
                "" if mean is None else f"{mean:.6f}",
                "" if rms is None else f"{rms:.6f}",
            ])

    row = [
        str(left_dir),
        str(right_dir),
        str(len(matched_ids)),
        str(left_unmatched),
        str(right_unmatched),
        "" if all_mean is None else f"{all_mean:.6f}",
        "" if all_rms is None else f"{all_rms:.6f}",
    ] + bin_values

    if args.header:
        header = [
            "left",
            "right",
            "num_matching",
            "left_unmatched",
            "right_unmatched",
            "all_mean",
            "all_rms",
        ] + bin_headers
        print(",".join(header))

    print(",".join(row))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
