#!/usr/bin/env python3
"""Lightweight viewer app for inspecting station files and comparisons.

Run with a data directory, optional reference directory, and optional inventory.
Uses a tiny HTTP server with minimal JS UI.
"""
from __future__ import annotations

import argparse
import json
import math
import hashlib
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from urllib.parse import parse_qs, urlparse


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Viewer app for station file comparisons")
    parser.add_argument("--dir", required=True, help="Primary data directory")
    parser.add_argument("--ref", help="Reference data directory")
    parser.add_argument("--inventory", help="Station inventory file")
    parser.add_argument("--host", default="127.0.0.1", help="Host to bind")
    parser.add_argument("--port", type=int, default=8080, help="Port to bind")
    return parser.parse_args()


def list_station_files(directory: Path) -> Dict[str, Path]:
    mapping: Dict[str, Path] = {}
    for entry in directory.iterdir():
        if not entry.is_file():
            continue
        station_id = entry.name.split(".", 1)[0]
        if station_id:
            mapping[station_id] = entry
    return mapping


def hash_dir_state(directory: Path) -> str:
    h = hashlib.sha256()
    for entry in sorted(directory.iterdir()):
        if not entry.is_file():
            continue
        stat = entry.stat()
        h.update(entry.name.encode("utf-8"))
        h.update(str(stat.st_size).encode("utf-8"))
        h.update(str(int(stat.st_mtime)).encode("utf-8"))
    return h.hexdigest()


def hash_file_state(path: Optional[Path]) -> str:
    if path is None or not path.exists():
        return ""
    stat = path.stat()
    h = hashlib.sha256()
    h.update(path.name.encode("utf-8"))
    h.update(str(stat.st_size).encode("utf-8"))
    h.update(str(int(stat.st_mtime)).encode("utf-8"))
    return h.hexdigest()


def parse_station_file(path: Path) -> Dict[int, Tuple[List[int], List[str]]]:
    data: Dict[int, Tuple[List[int], List[str]]] = {}
    with path.open("rb") as fh:
        for line in fh:
            if len(line) < 16 + 12 * 9:
                continue
            try:
                year = int(line[12:16])
            except ValueError:
                continue
            values: List[int] = []
            qcflags: List[str] = []
            base = 16
            for month in range(12):
                offset = base + month * 9
                value_raw = line[offset:offset + 6].strip()
                flag = line[offset + 6:offset + 9]
                try:
                    value = int(value_raw) if value_raw else -9999
                except ValueError:
                    value = -9999
                qc = chr(flag[1]) if len(flag) >= 2 else ""
                values.append(value)
                qcflags.append(qc)
            data[year] = (values, qcflags)
    return data


def parse_inventory(path: Path) -> Dict[str, Tuple[float, float, str]]:
    inv: Dict[str, Tuple[float, float, str]] = {}
    with path.open("r", encoding="ascii", errors="replace") as fh:
        for line in fh:
            if len(line) < 30:
                continue
            station_id = line[0:11]
            try:
                lat = float(line[12:20])
                lon = float(line[21:30])
            except ValueError:
                continue
            name = line[38:68].strip() if len(line) >= 68 else ""
            inv[station_id] = (lat, lon, name)
    return inv


def grid_id(lat: float, lon: float) -> int:
    # Assume grid 1 at lat=0, lon=0; 3.75 deg lon x 2.5 deg lat.
    if lon < 0:
        lon += 360.0
    lat_index = int(math.floor((lat - 0.0) / 2.5))
    lon_index = int(math.floor((lon - 0.0) / 3.75))
    return lat_index * 96 + lon_index + 1


def should_include(qc: str, include_qc: bool) -> bool:
    if include_qc:
        return True
    return qc == " "


def compute_overall_mean(left_map: Dict[str, Path], include_qc: bool) -> Tuple[float, int]:
    total = 0.0
    count = 0
    for path in left_map.values():
        data = parse_station_file(path)
        for values, qcflags in data.values():
            for v, qc in zip(values, qcflags):
                if v == -9999:
                    continue
                if not should_include(qc, include_qc):
                    continue
                total += v / 100.0
                count += 1
    return total, count


def compute_overall_diff(
    left_map: Dict[str, Path],
    right_map: Dict[str, Path],
    include_qc: bool,
) -> Tuple[float, int]:
    total = 0.0
    count = 0
    for station_id, left_path in left_map.items():
        right_path = right_map.get(station_id)
        if right_path is None:
            continue
        left_data = parse_station_file(left_path)
        right_data = parse_station_file(right_path)
        for year in set(left_data.keys()) & set(right_data.keys()):
            lvals, lqc = left_data[year]
            rvals, rqc = right_data[year]
            for lv, rv, lq, rq in zip(lvals, rvals, lqc, rqc):
                if lv == -9999 or rv == -9999:
                    continue
                if not (should_include(lq, include_qc) and should_include(rq, include_qc)):
                    continue
                total += (lv - rv) / 100.0
                count += 1
    return total, count


def compute_grid_mean(
    left_map: Dict[str, Path],
    inv: Dict[str, Tuple[float, float, str]],
    include_qc: bool,
) -> Dict[int, Tuple[float, int]]:
    grid: Dict[int, Tuple[float, int]] = {}
    for station_id, path in left_map.items():
        if station_id not in inv:
            continue
        lat, lon, _ = inv[station_id]
        gid = grid_id(lat, lon)
        data = parse_station_file(path)
        for values, qcflags in data.values():
            for v, qc in zip(values, qcflags):
                if v == -9999:
                    continue
                if not should_include(qc, include_qc):
                    continue
                s, c = grid.get(gid, (0.0, 0))
                grid[gid] = (s + v / 100.0, c + 1)
    return grid


def compute_grid_diff(
    left_map: Dict[str, Path],
    right_map: Dict[str, Path],
    inv: Dict[str, Tuple[float, float, str]],
    include_qc: bool,
) -> Dict[int, Tuple[float, int]]:
    grid: Dict[int, Tuple[float, int]] = {}
    for station_id, left_path in left_map.items():
        right_path = right_map.get(station_id)
        if right_path is None:
            continue
        if station_id not in inv:
            continue
        lat, lon, _ = inv[station_id]
        gid = grid_id(lat, lon)
        left_data = parse_station_file(left_path)
        right_data = parse_station_file(right_path)
        for year in set(left_data.keys()) & set(right_data.keys()):
            lvals, lqc = left_data[year]
            rvals, rqc = right_data[year]
            for lv, rv, lq, rq in zip(lvals, rvals, lqc, rqc):
                if lv == -9999 or rv == -9999:
                    continue
                if not (should_include(lq, include_qc) and should_include(rq, include_qc)):
                    continue
                s, c = grid.get(gid, (0.0, 0))
                grid[gid] = (s + (lv - rv) / 100.0, c + 1)
    return grid


def time_key(year: int, month: int, granularity: str) -> str:
    if granularity == "yearly":
        return f"{year:04d}"
    return f"{year:04d}-{month:02d}"


def finalize_series(sum_count: Dict[str, Tuple[float, int]]) -> List[dict]:
    points = []
    for t in sorted(sum_count.keys()):
        s, c = sum_count[t]
        if c == 0:
            continue
        points.append({"t": t, "v": s / c})
    return points


def compute_overall_series(
    left_map: Dict[str, Path],
    include_qc: bool,
    granularity: str,
) -> List[dict]:
    sums: Dict[str, Tuple[float, int]] = {}
    for path in left_map.values():
        data = parse_station_file(path)
        for year, (values, qcflags) in data.items():
            for idx, (v, qc) in enumerate(zip(values, qcflags), start=1):
                if v == -9999:
                    continue
                if not should_include(qc, include_qc):
                    continue
                key = time_key(year, idx, granularity)
                s, c = sums.get(key, (0.0, 0))
                sums[key] = (s + v / 100.0, c + 1)
    return finalize_series(sums)


def compute_overall_series_compare(
    left_map: Dict[str, Path],
    right_map: Dict[str, Path],
    include_qc: bool,
    granularity: str,
) -> Tuple[List[dict], List[dict], List[dict]]:
    left_sums: Dict[str, Tuple[float, int]] = {}
    right_sums: Dict[str, Tuple[float, int]] = {}
    diff_sums: Dict[str, Tuple[float, int]] = {}
    for station_id, left_path in left_map.items():
        right_path = right_map.get(station_id)
        if right_path is None:
            continue
        left_data = parse_station_file(left_path)
        right_data = parse_station_file(right_path)
        for year in set(left_data.keys()) & set(right_data.keys()):
            lvals, lqc = left_data[year]
            rvals, rqc = right_data[year]
            for idx, (lv, rv, lq, rq) in enumerate(zip(lvals, rvals, lqc, rqc), start=1):
                if lv == -9999 or rv == -9999:
                    continue
                if not (should_include(lq, include_qc) and should_include(rq, include_qc)):
                    continue
                key = time_key(year, idx, granularity)
                ls, lc = left_sums.get(key, (0.0, 0))
                rs, rc = right_sums.get(key, (0.0, 0))
                ds, dc = diff_sums.get(key, (0.0, 0))
                left_sums[key] = (ls + lv / 100.0, lc + 1)
                right_sums[key] = (rs + rv / 100.0, rc + 1)
                diff_sums[key] = (ds + (lv - rv) / 100.0, dc + 1)
    return finalize_series(left_sums), finalize_series(right_sums), finalize_series(diff_sums)


def compute_grid_series(
    left_map: Dict[str, Path],
    inv: Dict[str, Tuple[float, float, str]],
    include_qc: bool,
    granularity: str,
) -> List[dict]:
    buckets: Dict[str, Dict[int, Tuple[float, int]]] = {}
    for station_id, path in left_map.items():
        if station_id not in inv:
            continue
        lat, lon, _ = inv[station_id]
        gid = grid_id(lat, lon)
        data = parse_station_file(path)
        for year, (values, qcflags) in data.items():
            for idx, (v, qc) in enumerate(zip(values, qcflags), start=1):
                if v == -9999:
                    continue
                if not should_include(qc, include_qc):
                    continue
                key = time_key(year, idx, granularity)
                grid_bucket = buckets.setdefault(key, {})
                s, c = grid_bucket.get(gid, (0.0, 0))
                grid_bucket[gid] = (s + v / 100.0, c + 1)
    series: Dict[str, Tuple[float, int]] = {}
    for t, grids in buckets.items():
        grid_means = [s / c for s, c in grids.values() if c > 0]
        if not grid_means:
            continue
        series[t] = (sum(grid_means), len(grid_means))
    return finalize_series(series)


def compute_grid_series_compare(
    left_map: Dict[str, Path],
    right_map: Dict[str, Path],
    inv: Dict[str, Tuple[float, float, str]],
    include_qc: bool,
    granularity: str,
) -> Tuple[List[dict], List[dict], List[dict]]:
    buckets_left: Dict[str, Dict[int, Tuple[float, int]]] = {}
    buckets_right: Dict[str, Dict[int, Tuple[float, int]]] = {}
    buckets_diff: Dict[str, Dict[int, Tuple[float, int]]] = {}
    for station_id, left_path in left_map.items():
        right_path = right_map.get(station_id)
        if right_path is None or station_id not in inv:
            continue
        lat, lon, _ = inv[station_id]
        gid = grid_id(lat, lon)
        left_data = parse_station_file(left_path)
        right_data = parse_station_file(right_path)
        for year in set(left_data.keys()) & set(right_data.keys()):
            lvals, lqc = left_data[year]
            rvals, rqc = right_data[year]
            for idx, (lv, rv, lq, rq) in enumerate(zip(lvals, rvals, lqc, rqc), start=1):
                if lv == -9999 or rv == -9999:
                    continue
                if not (should_include(lq, include_qc) and should_include(rq, include_qc)):
                    continue
                key = time_key(year, idx, granularity)
                bl = buckets_left.setdefault(key, {})
                br = buckets_right.setdefault(key, {})
                bd = buckets_diff.setdefault(key, {})
                ls, lc = bl.get(gid, (0.0, 0))
                rs, rc = br.get(gid, (0.0, 0))
                ds, dc = bd.get(gid, (0.0, 0))
                bl[gid] = (ls + lv / 100.0, lc + 1)
                br[gid] = (rs + rv / 100.0, rc + 1)
                bd[gid] = (ds + (lv - rv) / 100.0, dc + 1)
    def finalize(buckets: Dict[str, Dict[int, Tuple[float, int]]]) -> List[dict]:
        series: Dict[str, Tuple[float, int]] = {}
        for t, grids in buckets.items():
            grid_means = [s / c for s, c in grids.values() if c > 0]
            if not grid_means:
                continue
            series[t] = (sum(grid_means), len(grid_means))
        return finalize_series(series)
    return finalize(buckets_left), finalize(buckets_right), finalize(buckets_diff)


def count_breakpoints(
    left_path: Path,
    right_path: Path,
    include_qc: bool,
) -> int:
    left_data = parse_station_file(left_path)
    right_data = parse_station_file(right_path)
    prev_diff: Optional[int] = None
    count = 0
    for year in sorted(set(left_data.keys()) & set(right_data.keys())):
        lvals, lqc = left_data[year]
        rvals, rqc = right_data[year]
        for lv, rv, lq, rq in zip(lvals, rvals, lqc, rqc):
            if lv == -9999 or rv == -9999:
                continue
            if not (should_include(lq, include_qc) and should_include(rq, include_qc)):
                continue
            diff = lv - rv
            if prev_diff is None:
                prev_diff = diff
                continue
            if diff != prev_diff:
                count += 1
                prev_diff = diff
    return count


class ViewerApp:
    def __init__(self, left_dir: Path, right_dir: Optional[Path], inventory: Optional[Path]):
        self.left_dir = left_dir
        self.right_dir = right_dir
        self.left_map = list_station_files(left_dir)
        self.right_map = list_station_files(right_dir) if right_dir else {}
        self.inv = parse_inventory(inventory) if inventory else {}
        self.left_hash = hash_dir_state(left_dir)
        self.right_hash = hash_dir_state(right_dir) if right_dir else ""
        self.inv_hash = hash_file_state(inventory) if inventory else ""
        self._lock = threading.Lock()
        self._cache: Dict[str, dict] = {}
        self._series_lock = threading.Lock()
        self._series_cache: Dict[str, dict] = {}
        self._precompute_thread = threading.Thread(target=self._precompute_series, daemon=True)
        self._precompute_thread.start()

    def _series_cache_key(self, mode: str, include_qc: bool, granularity: str, compare: bool) -> str:
        return f"{mode}:{include_qc}:{granularity}:{compare}"

    def _precompute_series(self) -> None:
        total = 0
        done = 0
        compare_flags = [True] if self.right_dir else [False]
        for include_qc in (False, True):
            for granularity in ("yearly", "monthly"):
                for mode in ("overall", "grid"):
                    if mode == "grid" and not self.inv:
                        continue
                    for _ in compare_flags:
                        total += 1
        for include_qc in (False, True):
            for granularity in ("yearly", "monthly"):
                for mode in ("overall", "grid"):
                    if mode == "grid" and not self.inv:
                        continue
                    for compare in compare_flags:
                        key = self._series_cache_key(mode, include_qc, granularity, compare)
                        with self._series_lock:
                            if key in self._series_cache:
                                continue
                        result = self._compute_series_uncached(mode, include_qc, granularity, compare)
                        with self._series_lock:
                            self._series_cache[key] = result
                        done += 1
                        print(f"[viewer] precompute {done}/{total} ready: {key}")

    def _compute_series_uncached(self, mode: str, include_qc: bool, granularity: str, compare: bool) -> dict:
        if mode == "overall":
            if compare:
                left, right, diff = compute_overall_series_compare(self.left_map, self.right_map, include_qc, granularity)
                return {"series": [{"label": "left", "points": left}, {"label": "right", "points": right}], "diff": diff}
            return {"series": [{"label": "mean", "points": compute_overall_series(self.left_map, include_qc, granularity)}]}
        if mode == "grid":
            if not self.inv:
                return {"error": "inventory required"}
            if compare:
                left, right, diff = compute_grid_series_compare(self.left_map, self.right_map, self.inv, include_qc, granularity)
                return {"series": [{"label": "left", "points": left}, {"label": "right", "points": right}], "diff": diff}
            return {"series": [{"label": "grid-mean", "points": compute_grid_series(self.left_map, self.inv, include_qc, granularity)}]}
        return {"error": "unknown mode"}

    def get_station_list(self, include_qc: bool) -> List[dict]:
        key = f"stations:{include_qc}"
        with self._lock:
            if key in self._cache:
                return self._cache[key]
        stations = []
        for station_id in sorted(self.left_map.keys()):
            has_ref = station_id in self.right_map
            bps = None
            if has_ref:
                bps = count_breakpoints(self.left_map[station_id], self.right_map[station_id], include_qc)
            name = ""
            if station_id in self.inv:
                _, _, name = self.inv[station_id]
            stations.append({"id": station_id, "name": name, "has_ref": has_ref, "breakpoints": bps})
        with self._lock:
            self._cache[key] = stations
        return stations

    def summary(self, mode: str, include_qc: bool) -> dict:
        key = f"summary:{mode}:{include_qc}"
        with self._lock:
            if key in self._cache:
                return self._cache[key]

        if mode == "overall":
            total, count = compute_overall_mean(self.left_map, include_qc)
            result = {"mean": (total / count if count else None), "count": count}
        elif mode == "overall_ref":
            total, count = compute_overall_diff(self.left_map, self.right_map, include_qc)
            result = {"mean": (total / count if count else None), "count": count}
        elif mode == "grid":
            if not self.inv:
                result = {"error": "inventory required"}
            else:
                grid = compute_grid_mean(self.left_map, self.inv, include_qc)
                result = {"grid": {str(k): {"mean": (v[0] / v[1] if v[1] else None), "count": v[1]} for k, v in grid.items()}}
        elif mode == "grid_ref":
            if not self.inv:
                result = {"error": "inventory required"}
            else:
                grid = compute_grid_diff(self.left_map, self.right_map, self.inv, include_qc)
                result = {"grid": {str(k): {"mean": (v[0] / v[1] if v[1] else None), "count": v[1]} for k, v in grid.items()}}
        else:
            result = {"error": "unknown mode"}

        with self._lock:
            self._cache[key] = result
        return result

    def station_data(self, station_id: str, include_qc: bool, compare: bool) -> dict:
        if station_id not in self.left_map:
            return {"error": "station not found"}
        left_data = parse_station_file(self.left_map[station_id])
        right_data = parse_station_file(self.right_map[station_id]) if compare and station_id in self.right_map else None

        rows = []
        for year in sorted(left_data.keys()):
            lvals, lqc = left_data[year]
            rvals = None
            rqc = None
            if right_data and year in right_data:
                rvals, rqc = right_data[year]
            row = {"year": year, "values": [], "diffs": []}
            for m in range(12):
                lv = lvals[m]
                lq = lqc[m]
                if lv == -9999 or not should_include(lq, include_qc):
                    row["values"].append(None)
                else:
                    row["values"].append(lv / 100.0)
                if rvals is not None and rqc is not None:
                    rv = rvals[m]
                    rq = rqc[m]
                    if rv == -9999 or not should_include(rq, include_qc) or lv == -9999 or not should_include(lq, include_qc):
                        row["diffs"].append(None)
                    else:
                        row["diffs"].append((lv - rv) / 100.0)
            rows.append(row)
        return {"rows": rows, "has_ref": right_data is not None}

    def series(self, mode: str, include_qc: bool, granularity: str, station_id: Optional[str], compare: bool) -> dict:
        if mode == "overall":
            key = self._series_cache_key(mode, include_qc, granularity, compare)
            with self._series_lock:
                cached = self._series_cache.get(key)
            if cached is not None:
                return cached
            result = self._compute_series_uncached(mode, include_qc, granularity, compare)
            with self._series_lock:
                self._series_cache[key] = result
            return result
        if mode == "grid":
            key = self._series_cache_key(mode, include_qc, granularity, compare)
            with self._series_lock:
                cached = self._series_cache.get(key)
            if cached is not None:
                return cached
            result = self._compute_series_uncached(mode, include_qc, granularity, compare)
            with self._series_lock:
                self._series_cache[key] = result
            return result
        if mode == "station":
            if station_id is None or station_id == "":
                return {"error": "station id required"}
            station_ids = [s for s in station_id.split(",") if s]
            left_sums: Dict[str, Tuple[float, int]] = {}
            right_sums: Dict[str, Tuple[float, int]] = {}
            diff_sums: Dict[str, Tuple[float, int]] = {}
            has_ref_any = False
            for sid in station_ids:
                if sid not in self.left_map:
                    continue
                left_data = parse_station_file(self.left_map[sid])
                right_data = parse_station_file(self.right_map[sid]) if compare and sid in self.right_map else None
                has_ref_any = has_ref_any or (right_data is not None)
                for year, (lvals, lqc) in left_data.items():
                    rvals = None
                    rqc = None
                    if right_data and year in right_data:
                        rvals, rqc = right_data[year]
                    for idx, (lv, lq) in enumerate(zip(lvals, lqc), start=1):
                        if lv == -9999 or not should_include(lq, include_qc):
                            continue
                        key = time_key(year, idx, granularity)
                        ls, lc = left_sums.get(key, (0.0, 0))
                        left_sums[key] = (ls + lv / 100.0, lc + 1)
                        if compare and rvals is not None and rqc is not None:
                            rv = rvals[idx - 1]
                            rq = rqc[idx - 1]
                            if rv == -9999 or not should_include(rq, include_qc):
                                continue
                            rs, rc = right_sums.get(key, (0.0, 0))
                            ds, dc = diff_sums.get(key, (0.0, 0))
                            right_sums[key] = (rs + rv / 100.0, rc + 1)
                            diff_sums[key] = (ds + (lv - rv) / 100.0, dc + 1)
            if not left_sums:
                return {"error": "no station data"}
            left_points = finalize_series(left_sums)
            series = [{"label": "left", "points": left_points}]
            if compare and has_ref_any:
                right_points = finalize_series(right_sums)
                diff_points = finalize_series(diff_sums)
                series.append({"label": "right", "points": right_points})
                return {"series": series, "diff": diff_points}
            return {"series": series}
        return {"error": "unknown mode"}


HTML_TEMPLATE = """<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <title>PHA Viewer</title>
  <style>
    body { font-family: sans-serif; margin: 20px; }
    .row { margin-bottom: 12px; }
    canvas { border: 1px solid #ddd; margin-bottom: 10px; }
    .btn-group { display: inline-flex; margin-left: 8px; }
    .btn {
      border: 1px solid #999;
      background: #eee;
      color: #333;
      padding: 6px 10px;
      cursor: pointer;
      margin-right: -1px;
    }
    .btn:first-child { border-top-left-radius: 4px; border-bottom-left-radius: 4px; }
    .btn:last-child { border-top-right-radius: 4px; border-bottom-right-radius: 4px; margin-right: 0; }
    .btn.active { background: #2f6db3; color: #fff; border-color: #2f6db3; }
    .btn:disabled { opacity: 0.5; cursor: not-allowed; }
    .plot-wrap { position: relative; display: inline-block; }
    .overlay {
      position: absolute;
      inset: 0;
      background: rgba(255,255,255,0.7);
      display: none;
      align-items: center;
      justify-content: center;
      z-index: 2;
    }
    .spinner {
      width: 28px;
      height: 28px;
      border: 4px solid #cfd6df;
      border-top-color: #2f6db3;
      border-radius: 50%;
      animation: spin 0.8s linear infinite;
    }
    @keyframes spin { to { transform: rotate(360deg); } }
  </style>
</head>
<body>
  <div class="plot-wrap">
    <canvas id="chart" width="900" height="300"></canvas>
    <canvas id="diff" width="900" height="300"></canvas>
    <div id="chartLoading" class="overlay"><div class="spinner"></div></div>
  </div>

  <div class="row">
    <div class="btn-group">
      <button class="btn active" id="qcIgnore" data-qc="ignore">Ignore QC</button>
      <button class="btn" id="qcInclude" data-qc="include">Include QC</button>
    </div>
  <div class="btn-group">
    <button class="btn" id="granMonthly" data-granularity="monthly">Monthly</button>
    <button class="btn active" id="granYearly" data-granularity="yearly">Yearly</button>
  </div>
  <div class="btn-group">
    <button class="btn" id="modeCompare" data-mode="compare">Compare</button>
    <button class="btn active" id="modeInspect" data-mode="inspect">Inspect</button>
  </div>
    <div class="btn-group">
      <button class="btn active" id="aggNumerical" data-agg="overall">Numerical</button>
      <button class="btn" id="aggGridded" data-agg="grid">Gridded</button>
    </div>
    <div class="btn-group">
      <button class="btn active" id="scopeAll" data-scope="all">All</button>
      <button class="btn" id="scopeSelection" data-scope="selection">Selection</button>
    </div>
  </div>
  <div class="row" style="position: relative;">
    <input id="stationFilter" placeholder="Filter stations..." />
    <div class="btn-group">
      <button class="btn active" id="sortId" data-sort="id">ID</button>
      <button class="btn" id="sortBpAsc" data-sort="bp_asc">BP ↑</button>
      <button class="btn" id="sortBpDesc" data-sort="bp_desc">BP ↓</button>
    </div>
    <select id="stationSelect" size="6" multiple></select>
    <button id="selectAllBtn" onclick="selectAllStations()" disabled>Select all</button>
    <span id="stationCount" class="small"></span>
    <div id="stationLoading" class="overlay"><div class="spinner"></div></div>
  </div>

  <script>
    const HAS_REF = __HAS_REF__;
    let currentView = {mode: 'overall', compare: true, stationIds: []};
    function setActiveButtons() {
      const toggleGroup = (ids, activeId) => {
        ids.forEach(id => document.getElementById(id).classList.remove('active'));
        document.getElementById(activeId).classList.add('active');
      };
      toggleGroup(['modeCompare', 'modeInspect'], currentView.compare ? 'modeCompare' : 'modeInspect');
      toggleGroup(['aggNumerical', 'aggGridded'], currentView.mode === 'grid' ? 'aggGridded' : 'aggNumerical');
      toggleGroup(['scopeAll', 'scopeSelection'], currentView.mode === 'station' ? 'scopeSelection' : 'scopeAll');
    }
    function includeQc() {
      return document.getElementById('qcInclude').classList.contains('active') ? '1' : '0';
    }
    function granularity() {
      return document.getElementById('granYearly').classList.contains('active') ? 'yearly' : 'monthly';
    }
    function drawSeries(canvasId, seriesList, yLabel) {
      const canvas = document.getElementById(canvasId);
      const ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      if (!seriesList || seriesList.length === 0) return;
      const allPoints = seriesList.flatMap(s => s.points);
      if (allPoints.length === 0) return;
      function parseYearFraction(t) {
        if (!t) return null;
        const parts = t.split('-');
        const year = parseInt(parts[0], 10);
        if (isNaN(year)) return null;
        if (parts.length > 1) {
          const month = parseInt(parts[1], 10);
          if (!isNaN(month)) {
            return year + (month - 1) / 12.0;
          }
        }
        return year;
      }
      const ys = allPoints.map(p => p.v);
      const minY = Math.min(...ys);
      const maxY = Math.max(...ys);
      const pad = { left: 58, right: 20, top: 25, bottom: 40 };
      const w = canvas.width - pad.left - pad.right;
      const h = canvas.height - pad.top - pad.bottom;
      function yToPx(y) {
        if (maxY === minY) return pad.top + h / 2;
        return pad.top + (maxY - y) / (maxY - minY) * h;
      }
      // Axes
      ctx.strokeStyle = '#999';
      ctx.beginPath();
      ctx.moveTo(pad.left, pad.top);
      ctx.lineTo(pad.left, pad.top + h);
      ctx.lineTo(pad.left + w, pad.top + h);
      ctx.stroke();

      // Y label
      ctx.fillStyle = '#333';
      ctx.font = '12px sans-serif';
      ctx.save();
      ctx.translate(18, pad.top + h / 2);
      ctx.rotate(-Math.PI / 2);
      ctx.fillText(yLabel, 0, 0);
      ctx.restore();

      // Y ticks
      const yTicks = 5;
      ctx.fillStyle = '#555';
      for (let i = 0; i <= yTicks; i++) {
        const y = pad.top + (i / yTicks) * h;
        ctx.beginPath();
        ctx.moveTo(pad.left - 4, y);
        ctx.lineTo(pad.left, y);
        ctx.stroke();
        const val = maxY - (i / yTicks) * (maxY - minY);
        const label = val.toFixed(1);
        const labelWidth = ctx.measureText(label).width;
        ctx.fillText(label, pad.left - 8 - labelWidth, y + 4);
      }

      // X labels (30-year ticks)
      const yearVals = allPoints.map(p => parseYearFraction(p.t)).filter(y => y !== null);
      const years = yearVals.map(y => Math.floor(y));
      if (years.length > 0) {
        const axisMin = Math.min(...yearVals);
        const axisMax = Math.max(...yearVals);
        const minYear = Math.floor(axisMin);
        const maxYear = Math.ceil(axisMax);
        const denom = (axisMax - axisMin) || 1;
        const anchor = 1700;
        const offset = Math.ceil((axisMin - anchor) / 30) * 30;
        const startTick = anchor + offset;
        for (let y = startTick; y <= maxYear; y += 30) {
          const x = pad.left + ((y - axisMin) / denom) * w;
          ctx.beginPath();
          ctx.moveTo(x, pad.top + h);
          ctx.lineTo(x, pad.top + h + 4);
          ctx.stroke();
          ctx.fillText(String(y), x - 12, pad.top + h + 16);
        }
      }
      seriesList.forEach((series, idx) => {
        ctx.strokeStyle = idx === 0 ? '#1f77b4' : '#ff7f0e';
        ctx.beginPath();
        series.points.forEach((p, i) => {
          const yr = parseYearFraction(p.t);
          const axisMin = Math.min(...yearVals);
          const axisMax = Math.max(...yearVals);
          const denom = (axisMax - axisMin) || 1;
          const t = yr === null ? i / (series.points.length - 1 || 1) : (yr - axisMin) / denom;
          const x = pad.left + t * w;
          const y = yToPx(p.v);
          if (i === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
        });
        ctx.stroke();
      });
    }
    function adjustPlotLayout(compare) {
      const chart = document.getElementById('chart');
      const diff = document.getElementById('diff');
      if (compare) {
        chart.height = 300;
        diff.style.display = 'block';
        diff.height = 300;
      } else {
        chart.height = 600;
        diff.style.display = 'none';
      }
    }

    const seriesCache = new Map();
    let chartRequestId = 0;
    async function fetchSeries(mode, compare) {
      const reqId = ++chartRequestId;
      const stationIds = currentView.stationIds || [];
      const stationParam = mode === 'station' ? stationIds.join(',') : '';
      const key = `${mode}|${compare ? 1 : 0}|${includeQc()}|${granularity()}|${stationParam}`;
      if (seriesCache.has(key)) return seriesCache.get(key);
      const loading = document.getElementById('chartLoading');
      loading.style.display = 'flex';
      const res = await fetch(`/api/series?mode=${mode}&compare=${compare ? '1' : '0'}&include_qc=${includeQc()}&granularity=${granularity()}&station_id=${stationParam}`);
      const data = await res.json();
      if (reqId === chartRequestId) {
        loading.style.display = 'none';
      }
      seriesCache.set(key, data);
      return data;
    }
    async function loadSeries(mode, compare) {
      const useCompare = compare || (HAS_REF && !compare);
      const data = await fetchSeries(mode, useCompare);
      adjustPlotLayout(compare);
      let series = data.series || [];
      if (!compare && HAS_REF) {
        const leftOnly = series.filter(s => s.label === 'left');
        series = leftOnly.length > 0 ? leftOnly : (series.length ? [series[0]] : []);
      }
      drawSeries('chart', series, 'T (°C)');
      if (compare) {
        drawSeries('diff', data.diff ? [{label:'diff', points:data.diff}] : [], 'ΔT (°C)');
      }
      if (HAS_REF) {
        fetchSeries(mode, true).catch(() => {});
      }
    }
    function selectView(mode, compare) {
      const stationIds = Array.from(document.getElementById('stationSelect').selectedOptions).map(o => o.value);
      currentView = {mode, compare, stationIds};
      setActiveButtons();
      loadSeries(mode, compare);
    }
    let stationCache = null;
    let stationRequestId = 0;
    async function fetchStations() {
      const reqId = ++stationRequestId;
      const loading = document.getElementById('stationLoading');
      loading.style.display = 'flex';
      const res = await fetch(`/api/stations?include_qc=${includeQc()}`);
      const data = await res.json();
      if (reqId === stationRequestId) {
        loading.style.display = 'none';
      }
      stationCache = data;
      return data;
    }
    function applyStations(data) {
      const filterText = (document.getElementById('stationFilter').value || '').toLowerCase();
      let sortMode = 'id';
      if (document.getElementById('sortBpAsc').classList.contains('active')) sortMode = 'bp_asc';
      if (document.getElementById('sortBpDesc').classList.contains('active')) sortMode = 'bp_desc';
      let rows = data.filter(s => {
        if (!filterText) return true;
        const name = (s.name || '').toLowerCase();
        return s.id.toLowerCase().includes(filterText) || name.includes(filterText);
      });
      if (sortMode === 'bp_asc') {
        rows.sort((a, b) => (a.breakpoints ?? 1e9) - (b.breakpoints ?? 1e9));
      } else if (sortMode === 'bp_desc') {
        rows.sort((a, b) => (b.breakpoints ?? -1) - (a.breakpoints ?? -1));
      } else {
        rows.sort((a, b) => a.id.localeCompare(b.id));
      }
      const sel = document.getElementById('stationSelect');
      sel.innerHTML = '';
      rows.forEach(s => {
        const opt = document.createElement('option');
        opt.value = s.id;
        const name = s.name ? ` - ${s.name}` : '';
        opt.textContent = `${s.id}${name} (bp=${s.breakpoints ?? 'n/a'})`;
        sel.appendChild(opt);
      });
      const hasStations = rows.length > 0;
      document.getElementById('selectAllBtn').disabled = !hasStations;
      const selCount = sel.selectedOptions.length;
      document.getElementById('stationCount').textContent = `selected ${selCount} / ${rows.length}`;
    }
    async function loadStations() {
      const data = stationCache || await fetchStations();
      applyStations(data);
    }
    function selectAllStations() {
      const sel = document.getElementById('stationSelect');
      for (const opt of sel.options) {
        opt.selected = true;
      }
      updateStationCount();
      refreshFromControls();
    }
    function updateStationCount() {
      const sel = document.getElementById('stationSelect');
      const total = sel.options.length;
      const selected = sel.selectedOptions.length;
      document.getElementById('stationCount').textContent = `selected ${selected} / ${total}`;
    }
    function refreshFromControls() {
      const compare = document.getElementById('modeCompare').classList.contains('active');
      const agg = document.getElementById('aggGridded').classList.contains('active') ? 'grid' : 'overall';
      const scope = document.getElementById('scopeSelection').classList.contains('active') ? 'station' : agg;
      selectView(scope, compare);
    }
    document.getElementById('qcIgnore').addEventListener('click', () => {
      document.getElementById('qcIgnore').classList.add('active');
      document.getElementById('qcInclude').classList.remove('active');
      loadStations();
      refreshFromControls();
    });
    document.getElementById('qcInclude').addEventListener('click', () => {
      document.getElementById('qcInclude').classList.add('active');
      document.getElementById('qcIgnore').classList.remove('active');
      loadStations();
      refreshFromControls();
    });
    document.getElementById('granMonthly').addEventListener('click', () => {
      document.getElementById('granMonthly').classList.add('active');
      document.getElementById('granYearly').classList.remove('active');
      refreshFromControls();
    });
    document.getElementById('granYearly').addEventListener('click', () => {
      document.getElementById('granYearly').classList.add('active');
      document.getElementById('granMonthly').classList.remove('active');
      refreshFromControls();
    });
    document.getElementById('modeCompare').addEventListener('click', () => { document.getElementById('modeCompare').classList.add('active'); document.getElementById('modeInspect').classList.remove('active'); refreshFromControls(); });
    document.getElementById('modeInspect').addEventListener('click', () => { document.getElementById('modeInspect').classList.add('active'); document.getElementById('modeCompare').classList.remove('active'); refreshFromControls(); });
    document.getElementById('aggNumerical').addEventListener('click', () => { document.getElementById('aggNumerical').classList.add('active'); document.getElementById('aggGridded').classList.remove('active'); refreshFromControls(); });
    document.getElementById('aggGridded').addEventListener('click', () => { document.getElementById('aggGridded').classList.add('active'); document.getElementById('aggNumerical').classList.remove('active'); refreshFromControls(); });
    document.getElementById('scopeAll').addEventListener('click', () => { document.getElementById('scopeAll').classList.add('active'); document.getElementById('scopeSelection').classList.remove('active'); refreshFromControls(); });
    document.getElementById('scopeSelection').addEventListener('click', () => { document.getElementById('scopeSelection').classList.add('active'); document.getElementById('scopeAll').classList.remove('active'); refreshFromControls(); });
    document.getElementById('stationFilter').addEventListener('input', () => {
      if (stationCache) applyStations(stationCache);
      else loadStations();
    });
    const sortButtons = ['sortId', 'sortBpAsc', 'sortBpDesc'];
    sortButtons.forEach(id => {
      document.getElementById(id).addEventListener('click', () => {
        sortButtons.forEach(other => document.getElementById(other).classList.remove('active'));
        document.getElementById(id).classList.add('active');
        if (stationCache) applyStations(stationCache);
        else loadStations();
      });
    });
    document.getElementById('stationSelect').addEventListener('change', () => {
      updateStationCount();
      document.getElementById('scopeSelection').classList.add('active');
      document.getElementById('scopeAll').classList.remove('active');
      refreshFromControls();
    });

    window.addEventListener('load', () => {
      loadStations();
      setActiveButtons();
      refreshFromControls();
    });
  </script>
</body>
</html>
"""


class Handler(BaseHTTPRequestHandler):
    app: ViewerApp

    def _etag_for(self, kind: str, params: dict) -> str:
        parts = [
            kind,
            self.app.left_hash,
            self.app.right_hash,
            self.app.inv_hash,
            json.dumps(params, sort_keys=True),
        ]
        h = hashlib.sha256("::".join(parts).encode("utf-8")).hexdigest()
        return f"W/\"{h}\""

    def _maybe_304(self, etag: str) -> bool:
        if self.headers.get("If-None-Match") == etag:
            self.send_response(304)
            self.send_header("ETag", etag)
            self.end_headers()
            return True
        return False

    def _send_json(self, payload: dict):
        body = json.dumps(payload).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("ETag", self._etag)
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        parsed = urlparse(self.path)
        if parsed.path == "/":
            has_ref = "true" if self.app.right_dir is not None else "false"
            body = HTML_TEMPLATE.replace("__HAS_REF__", has_ref).encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return

        if parsed.path == "/api/stations":
            qs = parse_qs(parsed.query)
            include_qc = qs.get("include_qc", ["0"])[0] == "1"
            params = {"include_qc": include_qc}
            self._etag = self._etag_for("stations", params)
            if self._maybe_304(self._etag):
                return
            stations = self.app.get_station_list(include_qc)
            self._send_json(stations)
            return

        if parsed.path == "/api/summary":
            qs = parse_qs(parsed.query)
            include_qc = qs.get("include_qc", ["0"])[0] == "1"
            mode = qs.get("mode", ["overall"])[0]
            params = {"include_qc": include_qc, "mode": mode}
            self._etag = self._etag_for("summary", params)
            if self._maybe_304(self._etag):
                return
            self._send_json(self.app.summary(mode, include_qc))
            return

        if parsed.path == "/api/series":
            qs = parse_qs(parsed.query)
            include_qc = qs.get("include_qc", ["0"])[0] == "1"
            mode = qs.get("mode", ["overall"])[0]
            compare = qs.get("compare", ["0"])[0] == "1"
            granularity = qs.get("granularity", ["monthly"])[0]
            station_id = qs.get("station_id", [None])[0]
            params = {
                "include_qc": include_qc,
                "mode": mode,
                "compare": compare,
                "granularity": granularity,
                "station_id": station_id or "",
            }
            self._etag = self._etag_for("series", params)
            if self._maybe_304(self._etag):
                return
            self._send_json(self.app.series(mode, include_qc, granularity, station_id, compare))
            return

        if parsed.path == "/api/station":
            qs = parse_qs(parsed.query)
            station_id = qs.get("id", [""])[0]
            include_qc = qs.get("include_qc", ["0"])[0] == "1"
            compare = qs.get("compare", ["0"])[0] == "1"
            params = {"include_qc": include_qc, "compare": compare, "station_id": station_id}
            self._etag = self._etag_for("station", params)
            if self._maybe_304(self._etag):
                return
            self._send_json(self.app.station_data(station_id, include_qc, compare))
            return

        self.send_response(404)
        self.end_headers()


def main() -> int:
    args = parse_args()
    left = Path(args.dir)
    right = Path(args.ref) if args.ref else None
    inv = Path(args.inventory) if args.inventory else None

    app = ViewerApp(left, right, inv)
    Handler.app = app

    server = HTTPServer((args.host, args.port), Handler)
    print(f"Viewer running at http://{args.host}:{args.port}")
    server.serve_forever()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
