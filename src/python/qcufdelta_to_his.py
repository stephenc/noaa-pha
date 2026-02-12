#!/usr/bin/env python3
"""Reconstruct Time-of-Observation histories from USHCN QCU/QCF residuals.

This implements the forward scanning algorithm:
- Phase 1: Basis vector generation
- Phase 2: Forward scanning with gap handling (≥12 months)
- Phase 2.5: Boundary refinement (backtrack to find actual shift point)
- Phase 2.6: Boundary timing validation (adjust ±3 months for perfect fit)
- Phase 2.7: Pathological segmentation fix (merge segments < 12 months, use best variance)
- Phase 3: Code selection with tie-breaking (fewest values → longest runs → common codes)
- Phase 4: Walk-back for TOB+PHA cases (gap ≥18 months between segments)
- Phase 5: Merge same-code segments (with MSHR awareness)
- Validation: Perfect fit + variance check

See qcufdelta_to_his.md for detailed theory.

Usage:
    python3 -u qcufdelta_to_his_v3.py --inv INVENTORY --qcu-dir QCU_DIR --qcf-dir QCF_DIR \
        --out-history-dir OUT_DIR --mshr-zip MSHR_ZIP --tob-bin TOB_BIN
"""

import argparse
import calendar
import datetime as dt
import os
import shutil
import subprocess
import sys
import tempfile
import time
import zipfile
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple


# ==============================================================================
# Constants
# ==============================================================================

MISSING = -9999
VALUE_SCALE = 100  # Temperature values in hundredths of degrees C

# Thresholds
MIN_SEGMENT_MONTHS = 10         # Minimum data points for variance fitting
MIN_PERFECT_FIT_MONTHS = 3      # Minimum for perfect fit test
MIN_REFINEMENT_MONTHS = 12      # Minimum months for boundary refinement
MAX_WALKBACK_MONTHS = 12        # Maximum months to walk back

# Quick filter
QUICK_FILTER_THRESHOLD = 6      # Distinct values in 24-month window

# TOB codes
# Note: 00HR and 24HR are both midnight - we only include 24HR
ALL_TOB_CODES = [f"{h:02d}HR" for h in range(1, 25)] + [
    "00RS", "00SR", "00SS", "TRID"
]
TOB_NO_BIAS_CODE = "24HR"
COMMON_TOB_CODES = ['07HR', '17HR', '08HR', '18HR', '24HR', '06HR', '16HR']


# ==============================================================================
# Data structures
# ==============================================================================

@dataclass
class InventoryEntry:
    """Station metadata from inventory file."""
    station_id: str
    lat: float
    lon: float
    elev: float


@dataclass
class MshrRecord:
    """MSHR history record."""
    station_id: str
    begin_date: dt.date
    end_date: dt.date
    source: int
    lat_deg: float
    lat_min: float
    lat_sec: float
    lon_deg: float
    lon_min: float
    lon_sec: float
    distance_and_direction: str
    elevation: int
    instr_height: str
    station_instr: List[str]
    relocation: str


@dataclass
class TobSegment:
    """Time-of-observation code segment."""
    start_month: int  # Absolute month index
    end_month: int
    tob_code: str
    variance_score: float
    data_count: int = 0  # Number of data points in segment
    include_in_his: bool = True  # Whether to include in .his output
    start_day: Optional[int] = None  # Day of month for start (1-31), None = day 1
    end_day: Optional[int] = None  # Day of month for end (1-31), None = last day of month


@dataclass
class CandidateCode:
    """Tracking structure for candidate TOB codes during forward scan."""
    code: str
    distinct_count: int
    values: Set[int]  # Set of residual values in cents
    residuals: List[int]  # Full list of residuals for boundary refinement


# ==============================================================================
# Utility functions (copied from v2)
# ==============================================================================

def log(msg: str, flush: bool = True):
    """Print message with optional flush."""
    print(msg, flush=flush)


def format_time(seconds: float) -> str:
    """Format seconds as human-readable time string."""
    if seconds < 60:
        return f"{seconds:.0f}s"
    elif seconds < 3600:
        return f"{seconds / 60:.1f}m"
    else:
        return f"{seconds / 3600:.1f}h"


def to_absolute_month(year: int, month: int) -> int:
    """Convert (year, month) to absolute month index."""
    return year * 12 + month


def from_absolute_month(abs_month: int) -> Tuple[int, int]:
    """Convert absolute month index to (year, month)."""
    year = abs_month // 12
    month = abs_month % 12
    if month == 0:
        year -= 1
        month = 12
    return year, month


def decimal_to_dms(value: float) -> Tuple[int, int, int]:
    """Convert decimal degrees to degrees, minutes, seconds."""
    sign = -1 if value < 0 else 1
    total_seconds = abs(value) * 3600.0
    degrees = int(total_seconds // 3600)
    rem = total_seconds - degrees * 3600
    minutes = int(rem // 60)
    seconds = int(round(rem - minutes * 60))
    if seconds == 60:
        seconds = 0
        minutes += 1
    if minutes == 60:
        minutes = 0
        degrees += 1
    return sign * degrees, minutes, seconds


def count_distinct_values_integer(residuals: List[float]) -> int:
    """Count distinct values using integer math to avoid floating point errors.

    Converts residuals to integer cents, then counts unique values.
    This ensures 0.14 - 0.07 = 0.07 exactly, not 0.070000000001.
    """
    if not residuals:
        return 0
    residual_cents = [int(round(r * 100)) for r in residuals]
    return len(set(residual_cents))


# ==============================================================================
# File parsing (copied from v2)
# ==============================================================================

def parse_inventory(inv_path: Path) -> Dict[str, InventoryEntry]:
    """Parse GHCN-M inventory file."""
    entries = {}
    with open(inv_path, 'r', encoding='utf-8', errors='replace') as f:
        for line in f:
            if len(line) < 37:
                continue
            station_id = line[0:11]
            try:
                lat = float(line[12:20])
                lon = float(line[21:30])
                elev = float(line[31:37])
            except ValueError:
                continue
            entries[station_id] = InventoryEntry(station_id, lat, lon, elev)
    return entries


def parse_station_data(file_path: Path) -> Dict[Tuple[int, int], int]:
    """Parse station data file.

    Returns dict mapping (year, month) -> value (in hundredths of degrees C).
    """
    data = {}
    if not file_path.exists():
        return data

    with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
        for line in f:
            if len(line) < 124:
                continue

            year_str = line[12:16]
            try:
                year = int(year_str)
            except ValueError:
                continue

            for month in range(1, 13):
                base = 16 + (month - 1) * 9
                value_str = line[base:base + 6].strip()
                try:
                    value = int(value_str)
                except ValueError:
                    continue

                if value != MISSING:
                    data[(year, month)] = value

    return data


def parse_date(yyyymmdd: str) -> Optional[dt.date]:
    """Parse YYYYMMDD date string."""
    if len(yyyymmdd) != 8 or not yyyymmdd.isdigit():
        return None
    year = int(yyyymmdd[0:4])
    month = int(yyyymmdd[4:6])
    day = int(yyyymmdd[6:8])
    try:
        return dt.date(year, month, day)
    except ValueError:
        return None


def read_mshr_records(zip_path: Path, station_ids: Set[str]) -> Dict[str, List[MshrRecord]]:
    """Read MSHR Enhanced Table records for specified stations."""
    records = defaultdict(list)

    with zipfile.ZipFile(zip_path) as zf:
        member_name = zf.namelist()[0]
        with zf.open(member_name) as fh:
            for raw in fh:
                try:
                    line = raw.decode('utf-8')
                except UnicodeDecodeError:
                    line = raw.decode('latin-1', errors='replace')

                if len(line) < 1414:
                    continue

                station_id = line[239:259].strip()
                if not station_id or station_id not in station_ids:
                    continue

                begin_raw = line[32:40].strip()
                end_raw = line[41:49].strip()
                begin_date = parse_date(begin_raw)
                end_date = parse_date(end_raw)
                if begin_date is None or end_date is None:
                    continue

                rec = MshrRecord(
                    station_id=station_id,
                    begin_date=begin_date,
                    end_date=end_date,
                    source=2,
                    lat_deg=0.0,
                    lat_min=0.0,
                    lat_sec=0.0,
                    lon_deg=0.0,
                    lon_min=0.0,
                    lon_sec=0.0,
                    distance_and_direction="           ",
                    elevation=0,
                    instr_height="    ",
                    station_instr=[],
                    relocation=""
                )
                records[station_id].append(rec)

    for station_id in records:
        records[station_id] = sorted(records[station_id],
                                    key=lambda r: (r.begin_date, r.end_date))

    return records


# ==============================================================================
# Phase 0: Quick Filter (copied from v2)
# ==============================================================================

def compute_residuals(qcu_data: Dict[Tuple[int, int], int],
                     qcf_data: Dict[Tuple[int, int], int]) -> Dict[Tuple[int, int], float]:
    """Compute R(t) = QCF(t) - QCU(t) in degrees C."""
    residuals = {}
    for (year, month), qcu_val in qcu_data.items():
        if (year, month) in qcf_data:
            qcf_val = qcf_data[(year, month)]
            residuals[(year, month)] = (qcf_val - qcu_val) / VALUE_SCALE
    return residuals


def needs_tob_reconstruction(residuals: Dict[Tuple[int, int], float]) -> bool:
    """Quick filter: does this station need TOB reconstruction?

    Returns True if any 24-month window contains > 6 distinct residual values.
    """
    if len(residuals) < 24:
        return False

    sorted_times = sorted(residuals.keys())

    for i in range(len(sorted_times) - 23):
        window_residuals = [residuals[sorted_times[j]]
                          for j in range(i, min(i + 24, len(sorted_times)))]

        distinct_count = count_distinct_values_integer(window_residuals)
        if distinct_count > QUICK_FILTER_THRESHOLD:
            return True

    return False


# ==============================================================================
# Phase 1: Generate Basis Vectors (copied from v2)
# ==============================================================================

def generate_single_code_his(station_id: str, inv_entry: InventoryEntry,
                            tob_code: str, record_start: Tuple[int, int],
                            record_end: Tuple[int, int]) -> str:
    """Generate a .his file with a single TOB code for the entire record."""
    lat_deg, lat_min, lat_sec = decimal_to_dms(inv_entry.lat)
    lon_deg, lon_min, lon_sec = decimal_to_dms(inv_entry.lon)
    elev = int(round(inv_entry.elev))

    begin_date = dt.date(record_start[0], record_start[1], 1)
    end_date = dt.date(record_end[0], record_end[1], 28)

    line = (
        f"2"
        f"{' ' * 12}"
        f" {begin_date.year:4d}{begin_date.month:02d}{begin_date.day:02d}"
        f" {end_date.year:4d}{end_date.month:02d}{end_date.day:02d}"
        f" {lat_deg:3.0f}{lat_min:3.0f}{lat_sec:3.0f}"
        f" {lon_deg:4.0f}{lon_min:3.0f}{lon_sec:3.0f}"
        f" {' ' * 11}"
        f" {elev:5d}"
        f"  {' ' * 4}"
        f" {tob_code:4s}"
        f"     {' ' * 66}"
    )
    return line


def run_tobmain(tob_bin: Path, station_id: str, qcu_file: Path, his_file: Path,
               work_dir: Path, inv_entry: InventoryEntry, verbose: bool = False,
               debug_dir: Optional[Path] = None) -> Optional[Dict[Tuple[int, int], int]]:
    """Run TOBMain with given .his file and return adjusted data."""
    raw_dir = work_dir / "raw" / "tavg"
    tob_dir = work_dir / "tob" / "tavg"
    hist_dir = work_dir / "history"
    raw_dir.mkdir(parents=True, exist_ok=True)
    tob_dir.mkdir(parents=True, exist_ok=True)
    hist_dir.mkdir(parents=True, exist_ok=True)

    shutil.copy(qcu_file, raw_dir / f"{station_id}.raw.tavg")
    shutil.copy(his_file, hist_dir / f"{station_id}.his")

    inv_file = work_dir / "station.inv"
    with open(inv_file, 'w') as f:
        f.write(f"{station_id:11s} {inv_entry.lat:8.4f}  {inv_entry.lon:9.4f}  {inv_entry.elev:6.1f} \n")

    props_file = work_dir / "tob.properties"
    with open(props_file, 'w') as f:
        f.write(f"""pha.element = tavg
pha.path.station-metadata = {inv_file}
pha.path.station-history = {hist_dir}/

tob.start-year = 1700
tob.start-from-history = true
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
        result = subprocess.run(
            [str(tob_bin), "-p", str(props_file)],
            cwd=work_dir,
            capture_output=True,
            text=True,
            timeout=30
        )
        if result.returncode != 0:
            if verbose:
                log(f"    TOBMain failed with return code {result.returncode}")
            if debug_dir:
                debug_station_dir = debug_dir / station_id
                debug_station_dir.mkdir(parents=True, exist_ok=True)
                for src in [props_file, inv_file, his_file]:
                    if src.exists():
                        shutil.copy(src, debug_station_dir / src.name)
            return None
    except (subprocess.TimeoutExpired, FileNotFoundError) as e:
        if verbose:
            log(f"    TOBMain exception: {e}")
        return None

    output_file = tob_dir / f"{station_id}.tob.tavg"
    if not output_file.exists():
        return None

    return parse_station_data(output_file)


def generate_basis_vectors(station_id: str, inv_entry: InventoryEntry,
                          qcu_data: Dict[Tuple[int, int], int],
                          residuals: Dict[Tuple[int, int], float],
                          tob_bin: Path, qcu_file: Path,
                          verbose: bool = False,
                          debug_dir: Optional[Path] = None) -> Dict[str, Dict[Tuple[int, int], float]]:
    """Generate basis vectors for all TOB codes."""

    if not residuals:
        return {}

    sorted_times = sorted(residuals.keys())
    record_start = sorted_times[0]
    record_end = sorted_times[-1]

    basis_vectors = {}

    with tempfile.TemporaryDirectory() as temp_dir:
        work_dir = Path(temp_dir)

        if verbose:
            log(f"  Generating basis vectors for {len(ALL_TOB_CODES)} TOB codes...")

        # Optimization: Batch all TOB codes in a single TOBMain run using synthetic station IDs
        # Instead of 30 forks per station, we do 1 fork for all 30 codes
        raw_dir = work_dir / "raw" / "tavg"
        tob_dir = work_dir / "tob" / "tavg"
        hist_dir = work_dir / "history"
        raw_dir.mkdir(parents=True, exist_ok=True)
        tob_dir.mkdir(parents=True, exist_ok=True)
        hist_dir.mkdir(parents=True, exist_ok=True)

        # Create synthetic station.inv with all 30 fake station IDs
        # Use format: first 9 chars of station_id + 2-digit index (stays within 11 char limit)
        inv_file = work_dir / "station.inv"
        code_to_index = {code: f"{i:02d}" for i, code in enumerate(ALL_TOB_CODES)}
        index_to_code = {idx: code for code, idx in code_to_index.items()}

        with open(inv_file, 'w') as f:
            for code in ALL_TOB_CODES:
                fake_id = f"{station_id[:9]}{code_to_index[code]}"
                f.write(f"{fake_id:11s} {inv_entry.lat:8.4f}  {inv_entry.lon:9.4f}  {inv_entry.elev:6.1f} \n")

        # Create .his files and copy/hardlink raw data for each synthetic station
        first_raw_file = None
        for i, code in enumerate(ALL_TOB_CODES):
            fake_id = f"{station_id[:9]}{code_to_index[code]}"

            # Create .his file with this TOB code
            his_content = generate_single_code_his(
                fake_id, inv_entry, code, record_start, record_end
            )
            his_file = hist_dir / f"{fake_id}.his"
            with open(his_file, 'w') as f:
                f.write(his_content + "\n")

            # Copy raw data once, then hardlink for remaining stations
            raw_file = raw_dir / f"{fake_id}.raw.tavg"
            if i == 0:
                # First file: copy from source to temp dir
                shutil.copy(qcu_file, raw_file)
                first_raw_file = raw_file
            else:
                # Remaining files: hardlink to first file (saves I/O and space)
                try:
                    os.link(first_raw_file, raw_file)
                except (OSError, AttributeError):
                    # Fallback if hardlinks not supported
                    shutil.copy(qcu_file, raw_file)

        # Run TOBMain ONCE for all 30 synthetic stations
        props_file = work_dir / "tob.properties"
        with open(props_file, 'w') as f:
            f.write(f"""pha.element = tavg
pha.path.station-metadata = {inv_file}
pha.path.station-history = {hist_dir}/

tob.start-year = 1700
tob.start-from-history = true
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
            result = subprocess.run(
                [str(tob_bin), "-p", str(props_file)],
                cwd=work_dir,
                capture_output=True,
                text=True,
                timeout=60  # Longer timeout for batch processing
            )
            if result.returncode != 0:
                if verbose:
                    log(f"    TOBMain batch failed with return code {result.returncode}")
                return {}
        except (subprocess.TimeoutExpired, FileNotFoundError) as e:
            if verbose:
                log(f"    TOBMain batch exception: {e}")
            return {}

        # Parse output from all 30 synthetic stations
        for code in ALL_TOB_CODES:
            fake_id = f"{station_id[:9]}{code_to_index[code]}"
            output_file = tob_dir / f"{fake_id}.tob.tavg"

            if not output_file.exists():
                if verbose:
                    log(f"    Failed to generate basis for {code}")
                continue

            tob_data = parse_station_data(output_file)
            if tob_data is None:
                continue

            basis = {}
            for (year, month), tob_val in tob_data.items():
                if (year, month) in qcu_data:
                    qcu_val = qcu_data[(year, month)]
                    basis[(year, month)] = (tob_val - qcu_val) / VALUE_SCALE

            basis_vectors[code] = basis

        if verbose:
            log(f"  Generated {len(basis_vectors)} basis vectors")

    return basis_vectors


# ==============================================================================
# Phase 2.5: Boundary Refinement
# ==============================================================================

def refine_boundary(segment_start: int, detected_boundary: int,
                   residuals: List[int]) -> int:
    """Find where residual distribution actually shifted.

    When code dies at detected_boundary, look backwards to find
    the month where the residual set changed from one stable
    distribution to another.

    Args:
        segment_start: First month index in residuals list
        detected_boundary: Index in residuals list where distinct count exceeded 3
        residuals: List of residual values (in cents) for this code

    Returns:
        Refined boundary index in residuals list (first month of new distribution)
    """
    # Need at least 12 months on each side to establish stable distribution
    if detected_boundary - segment_start < MIN_REFINEMENT_MONTHS:
        return detected_boundary

    if detected_boundary >= len(residuals):
        return detected_boundary

    # Scan backwards from detected_boundary
    for i in range(detected_boundary - 1, segment_start + MIN_REFINEMENT_MONTHS // 2, -1):
        # Compare residual sets before and after index i
        set_before = set(residuals[segment_start:i])
        set_after = set(residuals[i:detected_boundary])

        # Check if sets are disjoint or mostly disjoint
        # (at most 1 common value indicates a shift)
        overlap = len(set_before & set_after)

        if overlap <= 1 and len(set_before) >= 2 and len(set_after) >= 2:
            # Found the shift point - boundary is first month of new set
            return i

        # Alternative: check if variance changes significantly
        if len(set_before) >= 6 and len(set_after) >= 6:
            mean_before = sum(residuals[segment_start:i]) / (i - segment_start)
            mean_after = sum(residuals[i:detected_boundary]) / (detected_boundary - i)

            var_before = sum((r - mean_before)**2 for r in residuals[segment_start:i]) / (i - segment_start)
            var_after = sum((r - mean_after)**2 for r in residuals[i:detected_boundary]) / (detected_boundary - i)

            # If variance ratio > 2, distributions are different
            if var_before > 0 and var_after > 0:
                ratio = max(var_before, var_after) / min(var_before, var_after)
                if ratio > 2.0:
                    return i

    # If no clear shift found, use detected boundary
    # (May indicate gradual drift or complex transition)
    return detected_boundary


def count_segment_distinct_values(residuals: Dict[Tuple[int, int], float],
                                   basis: Dict[Tuple[int, int], float],
                                   start_month: int, end_month: int) -> int:
    """Count distinct residual values (in cents) for a segment."""
    residual_cents = []
    for (year, month), res_val in residuals.items():
        abs_month = to_absolute_month(year, month)
        if start_month <= abs_month <= end_month:
            if (year, month) in basis:
                cents = int(round((res_val - basis[(year, month)]) * 100))
                residual_cents.append(cents)

    if not residual_cents:
        return 0

    return len(set(residual_cents))


def validate_boundary_timing(segments: List[TobSegment],
                             residuals: Dict[Tuple[int, int], float],
                             basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
                             verbose: bool = False) -> List[TobSegment]:
    """Validate and adjust boundary timing to achieve perfect fit on both sides.

    After forward scan creates segments, check each boundary to see if adjusting
    it ±1-3 months would result in both adjacent segments having perfect fit
    (≤3 distinct values). This corrects for cases where the boundary refinement
    didn't find the exact transition month.

    Special case: If both segments use the same code after adjustment, this is
    likely a PHA-only step (not a TOB change), so mark as exclude from .his.
    """
    if len(segments) <= 1:
        return segments

    validated = [segments[0]]

    for i in range(1, len(segments)):
        seg = segments[i]
        prev_seg = validated[-1]

        # Count distinct values for both segments with current boundary
        prev_distinct = count_segment_distinct_values(
            residuals, basis_vectors[prev_seg.tob_code],
            prev_seg.start_month, prev_seg.end_month
        )
        curr_distinct = count_segment_distinct_values(
            residuals, basis_vectors[seg.tob_code],
            seg.start_month, seg.end_month
        )

        # Try adjusting boundary ±1, ±2, ±3 months to minimize total distinct values
        # Even if both segments have "perfect fit" (≤3 distinct), we should prefer
        # fewer total distinct values (e.g., 1+3=4 is better than 3+3=6)
        best_boundary = seg.start_month
        best_prev_distinct = prev_distinct
        best_curr_distinct = curr_distinct
        best_total = prev_distinct + curr_distinct

        for shift in [-3, -2, -1, 1, 2, 3]:
            new_boundary = seg.start_month + shift

            # Must leave at least MIN_SEGMENT_MONTHS in each segment
            if new_boundary <= prev_seg.start_month + MIN_SEGMENT_MONTHS:
                continue
            if new_boundary >= seg.end_month - MIN_SEGMENT_MONTHS:
                continue

            # Count distinct values with adjusted boundary
            new_prev_distinct = count_segment_distinct_values(
                residuals, basis_vectors[prev_seg.tob_code],
                prev_seg.start_month, new_boundary - 1
            )
            new_curr_distinct = count_segment_distinct_values(
                residuals, basis_vectors[seg.tob_code],
                new_boundary, seg.end_month
            )
            new_total = new_prev_distinct + new_curr_distinct

            # Accept adjustment if:
            # 1. Both segments achieve perfect fit, OR
            # 2. Total distinct values decreases
            if (new_prev_distinct <= 3 and new_curr_distinct <= 3 and
                not (best_prev_distinct <= 3 and best_curr_distinct <= 3)):
                # Achieved perfect fit on both sides
                best_boundary = new_boundary
                best_prev_distinct = new_prev_distinct
                best_curr_distinct = new_curr_distinct
                best_total = new_total
            elif new_total < best_total:
                # Improved total fit quality
                best_boundary = new_boundary
                best_prev_distinct = new_prev_distinct
                best_curr_distinct = new_curr_distinct
                best_total = new_total

        if best_boundary != seg.start_month:
            if verbose:
                old_y, old_m = from_absolute_month(seg.start_month)
                new_y, new_m = from_absolute_month(best_boundary)
                log(f"  Adjusted boundary: {old_y}-{old_m:02d} → {new_y}-{new_m:02d} "
                    f"(distinct: {prev_distinct}+{curr_distinct} → {best_prev_distinct}+{best_curr_distinct})")

            # Update previous segment end
            prev_seg_updated = TobSegment(
                start_month=prev_seg.start_month,
                end_month=best_boundary - 1,
                tob_code=prev_seg.tob_code,
                variance_score=prev_seg.variance_score,
                data_count=prev_seg.data_count,
                include_in_his=prev_seg.include_in_his
            )
            validated[-1] = prev_seg_updated

            # Update current segment start
            seg = TobSegment(
                start_month=best_boundary,
                end_month=seg.end_month,
                tob_code=seg.tob_code,
                variance_score=seg.variance_score,
                data_count=seg.data_count,
                include_in_his=seg.include_in_his
            )

        # Special case: If both segments use same code, this is likely PHA-only step
        if prev_seg.tob_code == seg.tob_code and best_prev_distinct <= 3 and best_curr_distinct <= 3:
            if verbose:
                start_y, start_m = from_absolute_month(seg.start_month)
                log(f"  Detected PHA-only step at {start_y}-{start_m:02d} (same code: {seg.tob_code})")
            # Mark second segment as PHA-only (exclude from .his)
            seg = TobSegment(
                start_month=seg.start_month,
                end_month=seg.end_month,
                tob_code=seg.tob_code,
                variance_score=seg.variance_score,
                data_count=seg.data_count,
                include_in_his=False  # PHA-only, exclude from .his
            )

        validated.append(seg)

    return validated


def refine_spike_boundaries(segments: List[TobSegment],
                            residuals: Dict[Tuple[int, int], float],
                            basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
                            verbose: bool = False) -> List[TobSegment]:
    """Refine boundaries with out-of-bound residual spikes using optimal day-of-month.

    Only applies to code-changing boundaries where the transition month has a residual
    outside the expected range of both codes. Searches for the optimal day within the
    month that minimizes the spike.

    Strategy:
    1. Prefer: day that gives 2 distinct values (within ±0.01°C tolerance)
    2. Fallback: day that gets closest to midpoint of both codes' residual ranges
    """
    if len(segments) <= 1:
        return segments

    refined = []

    for i, seg in enumerate(segments):
        if i == 0:
            refined.append(seg)
            continue

        prev_seg = refined[-1]

        # Only refine code-changing boundaries (not PHA-only)
        if prev_seg.tob_code == seg.tob_code:
            refined.append(seg)
            continue

        # Get the transition month (first month of current segment)
        transition_y, transition_m = from_absolute_month(seg.start_month)

        # Check if transition month exists and has residual
        if (transition_y, transition_m) not in residuals:
            refined.append(seg)
            continue

        # Check if we have basis vectors for both codes
        if (transition_y, transition_m) not in basis_vectors.get(prev_seg.tob_code, {}):
            refined.append(seg)
            continue
        if (transition_y, transition_m) not in basis_vectors.get(seg.tob_code, {}):
            refined.append(seg)
            continue

        transition_residual = residuals[(transition_y, transition_m)]
        diff_prev = transition_residual - basis_vectors[prev_seg.tob_code][(transition_y, transition_m)]
        diff_curr = transition_residual - basis_vectors[seg.tob_code][(transition_y, transition_m)]

        # Get residual ranges from nearby months (±6 months)
        prev_diffs = []
        curr_diffs = []
        for offset in range(-6, 7):
            if offset == 0:
                continue  # Skip transition month itself
            check_month = seg.start_month + offset
            check_y, check_m = from_absolute_month(check_month)
            if (check_y, check_m) in residuals:
                if (check_y, check_m) in basis_vectors.get(prev_seg.tob_code, {}):
                    prev_diffs.append(residuals[(check_y, check_m)] - basis_vectors[prev_seg.tob_code][(check_y, check_m)])
                if (check_y, check_m) in basis_vectors.get(seg.tob_code, {}):
                    curr_diffs.append(residuals[(check_y, check_m)] - basis_vectors[seg.tob_code][(check_y, check_m)])

        if not prev_diffs or not curr_diffs:
            refined.append(seg)
            continue

        # Check if transition month residual is out of bounds
        prev_min, prev_max = min(prev_diffs), max(prev_diffs)
        curr_min, curr_max = min(curr_diffs), max(curr_diffs)

        # Out of bound check (±0.01°C tolerance)
        TOLERANCE = 0.01
        prev_out_of_bound = (diff_prev < prev_min - TOLERANCE or diff_prev > prev_max + TOLERANCE)
        curr_out_of_bound = (diff_curr < curr_min - TOLERANCE or diff_curr > curr_max + TOLERANCE)

        # Only refine if BOTH are out of bound (spike affects both codes)
        if not (prev_out_of_bound and curr_out_of_bound):
            refined.append(seg)
            continue

        # Search for optimal day split
        _, days_in_month = calendar.monthrange(transition_y, transition_m)

        best_day = None
        best_distinct_count = float('inf')
        best_midpoint_distance = float('inf')

        midpoint = (prev_min + prev_max + curr_min + curr_max) / 4.0

        for split_day in range(2, days_in_month + 1):  # Days 2-31 (day 1 = no change)
            # Weighted average if split at this day
            days_with_prev = split_day - 1
            days_with_curr = days_in_month - split_day + 1
            weighted_diff = (days_with_prev * diff_prev + days_with_curr * diff_curr) / days_in_month

            # Count distinct values in nearby window
            weighted_cents = int(round(weighted_diff * 100))
            nearby_cents = {weighted_cents}

            # Add residuals from ±3 months
            for offset in range(-3, 4):
                if offset == 0:
                    continue
                check_month = seg.start_month + offset
                check_y, check_m = from_absolute_month(check_month)
                if (check_y, check_m) in residuals:
                    basis = basis_vectors[seg.tob_code] if offset > 0 else basis_vectors[prev_seg.tob_code]
                    if (check_y, check_m) in basis:
                        nearby_diff = residuals[(check_y, check_m)] - basis[(check_y, check_m)]
                        nearby_cents.add(int(round(nearby_diff * 100)))

            distinct_count = len(nearby_cents)
            midpoint_distance = abs(weighted_diff - midpoint)

            # Prefer 2 distinct values, then minimize midpoint distance
            if distinct_count < best_distinct_count or \
               (distinct_count == best_distinct_count and midpoint_distance < best_midpoint_distance):
                best_distinct_count = distinct_count
                best_midpoint_distance = midpoint_distance
                best_day = split_day

        # Apply the refinement if we found a better split
        if best_day is not None and best_day > 1:
            if verbose:
                log(f"  Refined spike boundary: {transition_y}-{transition_m:02d}-01 → {transition_y}-{transition_m:02d}-{best_day:02d} "
                    f"(spike: {diff_prev:+.3f}/{diff_curr:+.3f}°C, distinct: {best_distinct_count})")

            # Update previous segment to end on day before split
            prev_seg_updated = TobSegment(
                start_month=prev_seg.start_month,
                end_month=seg.start_month,  # Same month as current segment now
                tob_code=prev_seg.tob_code,
                variance_score=prev_seg.variance_score,
                data_count=prev_seg.data_count,
                include_in_his=prev_seg.include_in_his,
                start_day=prev_seg.start_day,
                end_day=best_day - 1  # End on day before split
            )
            refined[-1] = prev_seg_updated

            # Update current segment to start on split day
            seg_updated = TobSegment(
                start_month=seg.start_month,
                end_month=seg.end_month,
                tob_code=seg.tob_code,
                variance_score=seg.variance_score,
                data_count=seg.data_count,
                include_in_his=seg.include_in_his,
                start_day=best_day,
                end_day=seg.end_day
            )
            refined.append(seg_updated)
        else:
            refined.append(seg)

    return refined


# ==============================================================================
# Phase 3: Code Selection (Tie-Breaking)
# ==============================================================================

def choose_best_code(valid_codes: List[CandidateCode], previous_code: Optional[str]) -> str:
    """Choose among multiple valid codes.

    Priority:
    1. Fewest distinct values (1 better than 2 better than 3)
    2. Extends current run (matches previous_code)
    3. Most common code (e.g., 07HR, 17HR more likely than 21HR)
    4. First alphabetically (deterministic fallback)
    """
    if not valid_codes:
        return TOB_NO_BIAS_CODE

    # Group by distinct value count
    by_count = defaultdict(list)
    for candidate in valid_codes:
        by_count[candidate.distinct_count].append(candidate)

    # Take group with fewest distinct values
    min_count = min(by_count.keys())
    candidates = by_count[min_count]

    # If previous code is in candidates, prefer it (longest run)
    if previous_code:
        for candidate in candidates:
            if candidate.code == previous_code:
                return previous_code

    # Otherwise prefer common codes
    for code in COMMON_TOB_CODES:
        for candidate in candidates:
            if candidate.code == code:
                return code

    # Fallback: alphabetically first
    return sorted([c.code for c in candidates])[0]


# ==============================================================================
# Phase 4: Walk-Back for TOB+PHA Cases
# ==============================================================================

def is_tob_transition(residuals: Dict[Tuple[int, int], float],
                     start_month: int, end_month: int) -> bool:
    """Determine if a transition is TOB change vs PHA step.

    TOB change: Residual pattern (monthly structure) changes
    PHA step: Residual pattern stays same, but offset shifts

    Method:
    1. For each month in [start, end], group residuals by calendar month
    2. Check if residuals vary across same calendar month (e.g., all Januaries)
    3. If variance within calendar months > threshold: TOB change
    4. If variance is low but mean shifted: PHA step

    Returns:
        True if TOB transition, False if PHA step
    """
    # Group residuals by calendar month
    by_calendar_month = defaultdict(list)
    for (year, month), res_val in residuals.items():
        abs_month = to_absolute_month(year, month)
        if start_month <= abs_month <= end_month:
            by_calendar_month[month].append(res_val)

    # Check variance within each calendar month
    # TOB change → same calendar month has varying residuals
    # PHA step → same calendar month has constant residuals (just shifted)
    for cal_month, res_values in by_calendar_month.items():
        if len(res_values) >= 2:
            mean = sum(res_values) / len(res_values)
            variance = sum((r - mean)**2 for r in res_values) / len(res_values)
            # If any calendar month has significant variance, it's TOB change
            if variance > 0.01:  # More than 0.1°C std dev
                return True

    # Low variance → PHA step (constant pattern, shifted offset)
    return False


def find_perfect_codes(residuals: Dict[Tuple[int, int], float],
                      basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
                      start_month: int, end_month: int) -> List[CandidateCode]:
    """Find codes that give perfect fit (≤3 distinct values) in given range."""

    perfect_codes = []

    for code, basis in basis_vectors.items():
        diff_values = []
        for (year, month), res_val in residuals.items():
            abs_month = to_absolute_month(year, month)
            if start_month <= abs_month <= end_month:
                if (year, month) in basis:
                    diff_cents = int(round((res_val - basis[(year, month)]) * 100))
                    diff_values.append(diff_cents)

        if len(diff_values) < MIN_PERFECT_FIT_MONTHS:
            continue

        distinct_set = set(diff_values)
        if len(distinct_set) <= 3:
            perfect_codes.append(CandidateCode(
                code=code,
                distinct_count=len(distinct_set),
                values=distinct_set,
                residuals=diff_values
            ))

    return perfect_codes


def walk_back_and_split(segments: List[TobSegment],
                       residuals: Dict[Tuple[int, int], float],
                       basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
                       segment_end: int, next_segment_start: int,
                       verbose: bool = False) -> List[TobSegment]:
    """Walk backwards from stable zone to find TOB+PHA boundaries.

    Called when: gap between segments ≥18 months (segment_end to next_segment_start)

    Returns: List of new segments to insert (empty if walk-back doesn't find split)
    """
    gap_months = next_segment_start - segment_end - 1

    # Only walk back if gap is substantial (≥18 months)
    if gap_months < 18:
        return []

    last_segment = segments[-1] if segments else None
    if not last_segment:
        return []

    # Walk backwards from next_segment_start toward segment_end
    for months_back in range(1, min(MAX_WALKBACK_MONTHS + 1, gap_months)):
        t_back = next_segment_start - months_back

        # Check if [segment_end+1, t_back] has perfect fit
        perfect_codes = find_perfect_codes(residuals, basis_vectors, segment_end + 1, t_back)

        if perfect_codes:
            # Found split point
            split_month = t_back

            if verbose:
                split_y, split_m = from_absolute_month(split_month)
                log(f"  Walk-back found split at {split_y}-{split_m:02d}")

            # Determine which is TOB vs PHA by examining residual pattern
            if is_tob_transition(residuals, segment_end + 1, split_month):
                # First segment is TOB change - include in .his
                seg1_code = choose_best_code(perfect_codes, last_segment.tob_code)
                seg1 = TobSegment(
                    start_month=segment_end + 1,
                    end_month=split_month,
                    tob_code=seg1_code,
                    variance_score=0.0,  # Perfect fit
                    include_in_his=True  # TOB change
                )

                # Second segment is PHA step (same TOB code, shifted residuals)
                # Do NOT include in .his (PHA-only change)
                seg2 = TobSegment(
                    start_month=split_month + 1,
                    end_month=next_segment_start - 1,
                    tob_code=seg1_code,
                    variance_score=0.0,
                    include_in_his=False  # PHA-only, skip in .his output
                )

                if verbose:
                    log(f"    Split: TOB change then PHA step")
            else:
                # First segment is PHA step - do NOT include in .his
                seg1 = TobSegment(
                    start_month=segment_end + 1,
                    end_month=split_month,
                    tob_code=last_segment.tob_code,
                    variance_score=0.0,
                    include_in_his=False  # PHA-only, skip in .his output
                )

                # Second segment is TOB change - include in .his
                perfect_codes_2 = find_perfect_codes(residuals, basis_vectors,
                                                     split_month + 1, next_segment_start - 1)
                seg2_code = choose_best_code(perfect_codes_2, last_segment.tob_code) if perfect_codes_2 else TOB_NO_BIAS_CODE
                seg2 = TobSegment(
                    start_month=split_month + 1,
                    end_month=next_segment_start - 1,
                    tob_code=seg2_code,
                    variance_score=0.0,
                    include_in_his=True  # TOB change
                )

                if verbose:
                    log(f"    Split: PHA step then TOB change")

            # Return new segments to fill gap
            return [seg1, seg2]

    # If walk-back doesn't find split, return empty list
    return []


# ==============================================================================
# Self-Consistency Check
# ==============================================================================

def check_self_consistent_pattern(
    residuals_dict: Dict[Tuple[int, int], float],
    basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
    window_start: int,
    window_end: int,
    month_to_ym: Dict[int, Tuple[int, int]],
    verbose: bool = False
) -> Optional[str]:
    """Check if any code has a self-consistent repeating pattern by calendar month.

    A pattern is self-consistent if residuals for each calendar month are similar
    (low variance), even if different months have different values.

    This detects seasonal observation patterns that don't achieve perfect fit
    but are consistent year-over-year.

    Returns the best self-consistent code, or None.
    """
    # Need at least 12 months (1 year) to check for repeating pattern
    window_size = window_end - window_start + 1
    if window_size < 12:
        if verbose:
            log(f"    Self-consistency check skipped (window too small: {window_size} months)")
        return None

    # Count actual data points in window
    data_points = 0
    for abs_month in range(window_start, window_end + 1):
        if abs_month in month_to_ym:
            year, month = month_to_ym[abs_month]
            if (year, month) in residuals_dict:
                data_points += 1

    if verbose:
        log(f"    Checking self-consistency (window: {window_size} months, {data_points} data points)")

    best_code = None
    best_max_monthly_variance = float('inf')

    for code, basis in basis_vectors.items():
        # Group residuals by calendar month
        monthly_residuals = {m: [] for m in range(1, 13)}

        for abs_month in range(window_start, window_end + 1):
            if abs_month not in month_to_ym:
                continue

            year, month = month_to_ym[abs_month]
            if (year, month) not in residuals_dict or (year, month) not in basis:
                continue

            residual = residuals_dict[(year, month)] - basis[(year, month)]
            monthly_residuals[month].append(residual)

        # Check variance within each calendar month
        max_monthly_var = 0.0
        months_with_data = 0

        for month, vals in monthly_residuals.items():
            if len(vals) < 2:
                continue

            months_with_data += 1
            mean = sum(vals) / len(vals)
            variance = sum((x - mean)**2 for x in vals) / len(vals)
            max_monthly_var = max(max_monthly_var, variance)

        # Need data for enough calendar months (adaptive based on window size)
        # For 12-23 month window: need at least 4 months
        # For 24+ month window: need at least 6 months
        min_months_required = 4 if window_size < 24 else 6
        if months_with_data < min_months_required:
            continue

        # Self-consistent if max variance within any month is low
        # Adaptive threshold: stricter for dense data, more lenient for sparse data
        var_threshold = 0.010 if data_points < 36 else 0.005  # std dev ~0.10°C vs ~0.07°C
        if max_monthly_var < var_threshold and max_monthly_var < best_max_monthly_variance:
            best_max_monthly_variance = max_monthly_var
            best_code = code

    var_threshold = 0.010 if data_points < 36 else 0.005
    if verbose and best_code:
        log(f"    Self-consistent pattern found: {best_code} "
            f"(max monthly var={best_max_monthly_variance:.6f}, threshold={var_threshold})")
    elif verbose:
        log(f"    No self-consistent pattern (best max_var={best_max_monthly_variance:.6f}, threshold={var_threshold})")

    return best_code


# ==============================================================================
# Phase 2: Forward Scanning with Gap Handling
# ==============================================================================

def forward_scan(residuals: Dict[Tuple[int, int], float],
                basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
                verbose: bool = False) -> List[TobSegment]:
    """Phase 2: Forward scanning with gap handling.

    Core algorithm: Track codes that maintain ≤3 distinct values.
    When no codes remain valid, create boundary.
    """

    if not residuals:
        return []

    sorted_times = sorted(residuals.keys())
    if not sorted_times:
        return []

    # Map absolute month -> (year, month) for faster lookup
    month_to_ym = {to_absolute_month(y, m): (y, m) for y, m in sorted_times}
    absolute_months = sorted(month_to_ym.keys())

    segments = []
    window_start_idx = 0
    candidate_codes = list(ALL_TOB_CODES)
    last_valid_codes = None
    last_data_month = None

    # Track residuals for each code incrementally
    code_residuals = {code: [] for code in candidate_codes}

    for idx, abs_month in enumerate(absolute_months):
        year, month = month_to_ym[abs_month]

        # Check for gap ≥12 months - force boundary
        if last_data_month is not None and (abs_month - last_data_month) >= 12:
            if candidate_codes and last_valid_codes:
                prev_code = segments[-1].tob_code if segments else None
                chosen_code = choose_best_code(last_valid_codes, prev_code)

                segment = TobSegment(
                    start_month=absolute_months[window_start_idx],
                    end_month=last_data_month,
                    tob_code=chosen_code,
                    variance_score=0.0,  # Perfect fit
                    data_count=idx - window_start_idx
                )
                segments.append(segment)

                if verbose:
                    start_y, start_m = from_absolute_month(segment.start_month)
                    end_y, end_m = from_absolute_month(segment.end_month)
                    log(f"  Gap boundary at {end_y}-{end_m:02d}, code: {chosen_code}")

            # Start new segment after gap
            window_start_idx = idx
            candidate_codes = list(ALL_TOB_CODES)
            last_valid_codes = None
            code_residuals = {code: [] for code in candidate_codes}

        last_data_month = abs_month

        # For each candidate code, update residuals and check distinct values
        still_valid = []

        for code in candidate_codes:
            if code not in basis_vectors:
                continue

            basis = basis_vectors[code]
            if (year, month) not in basis:
                continue

            # Compute residual for this code at this month
            R = residuals[(year, month)]
            S = basis[(year, month)]
            residual_cents = int(round((R - S) * 100))

            code_residuals[code].append(residual_cents)

            # Count distinct values
            distinct_values = set(code_residuals[code])

            if len(distinct_values) <= 3:
                still_valid.append(CandidateCode(
                    code=code,
                    distinct_count=len(distinct_values),
                    values=distinct_values,
                    residuals=code_residuals[code].copy()
                ))

        if not still_valid:
            # Perfect fit broke - create boundary
            if last_valid_codes:
                # Refine boundary to find where shift actually occurred
                refined_idx = refine_boundary(
                    window_start_idx,
                    idx,
                    last_valid_codes[0].residuals
                )

                refined_boundary = absolute_months[refined_idx] if refined_idx < len(absolute_months) else abs_month

                # Choose code for segment [window_start, refined_boundary-1]
                prev_code = segments[-1].tob_code if segments else None
                chosen_code = choose_best_code(last_valid_codes, prev_code)

                segment = TobSegment(
                    start_month=absolute_months[window_start_idx],
                    end_month=refined_boundary - 1,
                    tob_code=chosen_code,
                    variance_score=0.0,  # Perfect fit
                    data_count=refined_idx - window_start_idx
                )
                segments.append(segment)

                if verbose:
                    start_y, start_m = from_absolute_month(segment.start_month)
                    end_y, end_m = from_absolute_month(segment.end_month)
                    log(f"  Boundary at {end_y}-{end_m:02d}, code: {chosen_code}")

                # Check if we need walk-back (gap between segments)
                gap_months = abs_month - refined_boundary
                if gap_months >= 18:
                    new_segments = walk_back_and_split(
                        segments, residuals, basis_vectors,
                        refined_boundary - 1, abs_month, verbose
                    )
                    segments.extend(new_segments)

            # Start new segment
            window_start_idx = idx
            candidate_codes = list(ALL_TOB_CODES)
            last_valid_codes = None
            code_residuals = {code: [] for code in candidate_codes}
        else:
            # Update candidates and save for potential boundary
            candidate_codes = [entry.code for entry in still_valid]
            last_valid_codes = still_valid

    # Handle final segment
    if candidate_codes and last_valid_codes:
        prev_code = segments[-1].tob_code if segments else None
        chosen_code = choose_best_code(last_valid_codes, prev_code)

        segment = TobSegment(
            start_month=absolute_months[window_start_idx],
            end_month=absolute_months[-1],
            tob_code=chosen_code,
            variance_score=0.0,  # Perfect fit
            data_count=len(absolute_months) - window_start_idx
        )
        segments.append(segment)

        if verbose:
            start_y, start_m = from_absolute_month(segment.start_month)
            end_y, end_m = from_absolute_month(segment.end_month)
            log(f"  Final segment: {start_y}-{start_m:02d} to {end_y}-{end_m:02d}, code: {chosen_code}")

    return segments


# ==============================================================================
# Monthly Hour Code Pattern Analysis
# ==============================================================================

def find_monthly_varying_solution(
    station_id: str,
    residuals: Dict[Tuple[int, int], float],
    basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
    verbose: bool = False
) -> Optional[Dict[int, str]]:
    """Find if a monthly-varying code pattern achieves consistent residuals.

    For each (calendar_month, code) combination, calculate the SET of residual values
    (rounded to cents). If 12 different (month, code) pairs produce the SAME set of
    values, that's a monthly-varying solution.

    Args:
        station_id: Station identifier
        residuals: QCF-QCU residuals
        basis_vectors: TOB basis vectors for each hour code
        verbose: Enable detailed logging

    Returns:
        Dict mapping calendar month (1-12) to hour code, or None if no pattern found
    """
    if verbose:
        log(f"  Searching for monthly-varying code solution...")

    # Map: residual_value → set of (calendar_month, code) pairs that produce it
    # We look at INDIVIDUAL residual values (±0.01), not sets
    # Only consider fixed hourly codes (00-24HR), not dynamic codes (RS, SR, SS, TRID)
    value_to_month_codes = {}

    for code, basis in basis_vectors.items():
        # Skip dynamic codes that already vary by month
        if code in ['00RS', '00SR', '00SS', 'TRID']:
            continue

        for cal_month in range(1, 13):
            # Get all residuals for this calendar month with this code
            for (year, month), res_val in residuals.items():
                if month == cal_month and (year, month) in basis:
                    adjusted = res_val - basis[(year, month)]
                    # Round to ±0.02°C precision (2-cent bins)
                    rounded_cents = round(adjusted * 50) * 2  # Bin into 2-cent increments

                    if rounded_cents not in value_to_month_codes:
                        value_to_month_codes[rounded_cents] = set()

                    # Add this (month, code) pair to this residual value
                    value_to_month_codes[rounded_cents].add((cal_month, code))

    # Find ALL residual values that can be achieved by 12 (month, code) pairs
    solutions = []
    best_match = None
    best_month_count = 0

    for residual_cents, month_code_pairs in value_to_month_codes.items():
        # Check how many distinct months and codes
        months_seen = {}  # month -> code (first code seen for this month)
        for cal_month, code in month_code_pairs:
            if cal_month not in months_seen:
                months_seen[cal_month] = code

        # Track best partial match for debugging
        if len(months_seen) > best_month_count:
            best_month_count = len(months_seen)
            codes_used = set(months_seen.values())
            best_match = (residual_cents / 100.0, len(months_seen), len(codes_used))

        # Check if we have all 12 months (codes can repeat!)
        if len(months_seen) == 12:
            codes_used = set(months_seen.values())
            solutions.append((residual_cents, len(codes_used), months_seen))

    if solutions:
        # Sort by number of distinct codes (prefer fewer codes)
        solutions.sort(key=lambda x: x[1])

        if verbose:
            import calendar
            log(f"    Found {len(solutions)} monthly-varying solution(s):")
            for i, (residual_cents, num_codes, months_seen) in enumerate(solutions):
                log(f"    Solution {i+1}: residual={residual_cents/100.0:+.2f}°C, {num_codes} distinct codes")
                for m in range(1, 13):
                    log(f"      {calendar.month_abbr[m]:3s}: {months_seen[m]}")
                if i < len(solutions) - 1:
                    log("")  # Blank line between solutions

        # Return the solution with fewest distinct codes
        return solutions[0][2]

    if verbose:
        if best_match:
            log(f"    No monthly-varying solution found")
            log(f"      Best match: residual={best_match[0]:+.2f}°C, {best_match[1]} months, {best_match[2]} codes")
        else:
            log(f"    No monthly-varying solution found (no residual values produced)")

    return None


def analyze_monthly_hour_pattern(
    station_id: str,
    residuals: Dict[Tuple[int, int], float],
    basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
    first_n_years: int = 5,
    verbose: bool = False
) -> Dict[int, Tuple[str, float, int]]:
    """Analyze if a monthly-varying hour code pattern can achieve perfect fit.

    For each calendar month (1-12), find which hour code achieves the best fit.
    This can help identify stations with seasonal observation time patterns.

    Args:
        station_id: Station identifier
        residuals: QCF-QCU residuals
        basis_vectors: TOB basis vectors for each hour code
        first_n_years: Limit analysis to first N years of data
        verbose: Enable detailed logging

    Returns:
        Dict mapping calendar month -> (best_code, variance, distinct_values)
    """
    if verbose:
        log(f"  Analyzing monthly hour code pattern for {station_id}...")

    # Filter to first N years
    sorted_times = sorted(residuals.keys())
    if not sorted_times:
        return {}

    first_year = sorted_times[0][0]
    last_year = first_year + first_n_years - 1

    limited_residuals = {
        (year, month): val for (year, month), val in residuals.items()
        if year <= last_year
    }

    if verbose:
        log(f"    Using data from {first_year} to {last_year} "
            f"({len(limited_residuals)} months)")

    monthly_best = {}

    for cal_month in range(1, 13):
        # Get residuals for this calendar month
        month_residuals = {
            (year, month): val for (year, month), val in limited_residuals.items()
            if month == cal_month
        }

        if len(month_residuals) < 3:
            continue

        best_code = None
        best_variance = float('inf')
        best_distinct = 999

        # Try all hour codes
        for code, basis in basis_vectors.items():
            diff_values = []
            for (year, month), res_val in month_residuals.items():
                if (year, month) in basis:
                    diff_values.append(res_val - basis[(year, month)])

            if len(diff_values) < 3:
                continue

            # Count distinct values (rounded to integer cents)
            distinct_ints = set(round(v * 100) for v in diff_values)
            distinct_count = len(distinct_ints)

            # Calculate variance
            mean = sum(diff_values) / len(diff_values)
            variance = sum((x - mean)**2 for x in diff_values) / len(diff_values)

            # Prefer codes with fewer distinct values, then lower variance
            if distinct_count < best_distinct or \
               (distinct_count == best_distinct and variance < best_variance):
                best_distinct = distinct_count
                best_variance = variance
                best_code = code

        if best_code:
            monthly_best[cal_month] = (best_code, best_variance, best_distinct)

    if verbose and monthly_best:
        import calendar
        log(f"    Monthly hour code pattern:")
        for month in range(1, 13):
            if month in monthly_best:
                code, var, distinct = monthly_best[month]
                log(f"      {calendar.month_abbr[month]:3s}: {code:5s}  "
                    f"distinct={distinct:2d}  var={var:.6f}")

    return monthly_best


# ==============================================================================
# Phase 3: Pathological Case Detection and Variance Fallback
# ==============================================================================

def detect_and_fix_pathological_segmentation(
    station_id: str,
    segments: List[TobSegment],
    residuals: Dict[Tuple[int, int], float],
    basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
    verbose: bool = False
) -> List[TobSegment]:
    """Detect and fix pathological segmentation with repeating annual patterns.

    Pathological case: A 12-month window with >1 segment (boundary within the year)
    that repeats in adjacent 12-month windows indicates fitting to seasonal noise
    rather than actual TOB regime changes.

    Solution: Identify regions where boundaries repeat annually, merge them, and
    replace with single segment using best-variance code.
    """
    if len(segments) <= 1:
        return segments

    # Group segments by calendar year and count segments per year
    year_segment_count = {}
    segment_to_year = {}

    for i, seg in enumerate(segments):
        start_year, start_month = from_absolute_month(seg.start_month)
        if start_year not in year_segment_count:
            year_segment_count[start_year] = []
        year_segment_count[start_year].append(i)
        segment_to_year[i] = start_year

    # Count how many years have >1 segment
    multi_segment_years = [year for year, segs in year_segment_count.items() if len(segs) > 1]

    # Mark segments that are part of repeating annual patterns
    # If ≥3 years have >1 segment, this indicates a repeating seasonal pattern
    is_pathological = [False] * len(segments)

    if len(multi_segment_years) >= 3:
        if verbose:
            log(f"  Repeating pattern detected: {len(multi_segment_years)} years with multiple segments")

        # Mark all segments from multi-segment years as pathological
        for year in multi_segment_years:
            for idx in year_segment_count[year]:
                is_pathological[idx] = True

    # If we detected repeating pattern, grow pathological regions using pattern matching
    # Start from each pathological segment and grow, stopping at PHA boundaries
    if any(is_pathological):
        pathological_indices = [i for i, val in enumerate(is_pathological) if val]

        if verbose:
            log(f"  Growing pathological regions using pattern matching ({len(pathological_indices)} initial segments)")

        # Build pathological regions by growing from each unprocessed pathological segment
        pathological_regions = []
        processed = set()

        for start_idx in pathological_indices:
            if start_idx in processed:
                continue

            # Start a new region from this segment
            # Build initial pattern from THIS segment only
            seg = segments[start_idx]
            pattern = {}
            for (year, month), res_val in residuals.items():
                abs_month = to_absolute_month(year, month)
                if seg.start_month <= abs_month <= seg.end_month:
                    if month not in pattern:
                        pattern[month] = res_val

            region_indices = [start_idx]
            processed.add(start_idx)

            # Grow backward from start_idx
            for i in range(start_idx - 1, -1, -1):
                seg = segments[i]

                # Get residuals for this segment in reverse chronological order
                seg_residuals = sorted(
                    ((abs_m, year, month, res_val) for (year, month), res_val in residuals.items()
                     if seg.start_month <= (abs_m := to_absolute_month(year, month)) <= seg.end_month),
                    reverse=True  # Backwards from segment end
                )

                # Try to match this segment's residuals to the pattern
                matches = True
                months_compared = 0

                for abs_month, year, month, res_val in seg_residuals:
                    if month in pattern:
                        # Calendar month seen before - must match within ±0.01°C
                        if abs(res_val - pattern[month]) > 0.01:
                            matches = False
                            if verbose:
                                log(f"  Pattern break backward at {year}-{month:02d}: {res_val:.3f} vs {pattern[month]:.3f}")
                            break
                        months_compared += 1
                    else:
                        # New calendar month - add to pattern
                        pattern[month] = res_val

                if matches and (months_compared > 0 or len(seg_residuals) > 0):
                    # Extend to include this segment
                    region_indices.insert(0, i)
                    if i in pathological_indices:
                        processed.add(i)
                else:
                    break

            # Grow forward from start_idx
            for i in range(start_idx + 1, len(segments)):
                seg = segments[i]

                # Get residuals for this segment in chronological order
                seg_residuals = sorted(
                    ((abs_m, year, month, res_val) for (year, month), res_val in residuals.items()
                     if seg.start_month <= (abs_m := to_absolute_month(year, month)) <= seg.end_month)
                )

                # Try to match this segment's residuals to the pattern
                matches = True
                months_compared = 0

                for abs_month, year, month, res_val in seg_residuals:
                    if month in pattern:
                        # Calendar month seen before - must match within ±0.01°C
                        if abs(res_val - pattern[month]) > 0.01:
                            matches = False
                            if verbose:
                                log(f"  Pattern break forward at {year}-{month:02d}: {res_val:.3f} vs {pattern[month]:.3f}")
                            break
                        months_compared += 1
                    else:
                        # New calendar month - add to pattern
                        pattern[month] = res_val

                if matches and (months_compared > 0 or len(seg_residuals) > 0):
                    # Extend to include this segment
                    region_indices.append(i)
                    if i in pathological_indices:
                        processed.add(i)
                else:
                    break

            # Store this pathological region
            pathological_regions.append(region_indices)

        # Clean up pathological regions:
        # 1. Remove regions < 12 months
        # 2. Fill gaps between adjacent regions
        # 3. Merge overlapping regions
        if verbose:
            log(f"  Cleaning up {len(pathological_regions)} pathological regions")

        cleaned_regions = []
        for region_indices in pathological_regions:
            region_start = segments[region_indices[0]].start_month
            region_end = segments[region_indices[-1]].end_month
            region_months = region_end - region_start + 1

            if region_months < 12:
                if verbose:
                    start_y, start_m = from_absolute_month(region_start)
                    end_y, end_m = from_absolute_month(region_end)
                    log(f"  Dropping tiny region {start_y}-{start_m:02d} to {end_y}-{end_m:02d} ({region_months} months)")
                continue

            cleaned_regions.append(region_indices)

        # Sort regions by start index
        cleaned_regions.sort(key=lambda r: r[0])

        # Fill gaps between adjacent regions - assign gap segments to nearest region
        final_regions = []
        for i, region_indices in enumerate(cleaned_regions):
            if i == 0:
                final_regions.append(region_indices)
                continue

            prev_region = final_regions[-1]
            curr_region = region_indices

            # Check if there's a gap between regions
            gap_start_idx = prev_region[-1] + 1
            gap_end_idx = curr_region[0] - 1

            if gap_start_idx <= gap_end_idx:
                # There's a gap - try to extend adjacent pathological regions into it
                gap_indices = list(range(gap_start_idx, gap_end_idx + 1))

                if verbose:
                    gap_start = segments[gap_start_idx].start_month
                    gap_end = segments[gap_end_idx].end_month
                    start_y, start_m = from_absolute_month(gap_start)
                    end_y, end_m = from_absolute_month(gap_end)
                    log(f"  Gap between regions: {start_y}-{start_m:02d} to {end_y}-{end_m:02d}")

                # Try to extend previous region forward into gap
                prev_extended = list(prev_region)
                for gap_idx in gap_indices:
                    seg = segments[gap_idx]
                    seg_residuals = sorted(
                        ((abs_m, year, month, res_val) for (year, month), res_val in residuals.items()
                         if seg.start_month <= (abs_m := to_absolute_month(year, month)) <= seg.end_month)
                    )

                    # Build pattern from prev region
                    pattern = {}
                    for idx in prev_extended:
                        s = segments[idx]
                        for (year, month), res_val in residuals.items():
                            abs_month = to_absolute_month(year, month)
                            if s.start_month <= abs_month <= s.end_month:
                                if month not in pattern:
                                    pattern[month] = res_val

                    # Check if gap segment matches pattern
                    matches = True
                    months_checked = 0
                    for abs_month, year, month, res_val in seg_residuals:
                        if month in pattern:
                            months_checked += 1
                            if abs(res_val - pattern[month]) > 0.01:
                                matches = False
                                if verbose:
                                    gap_start_y, gap_start_m = from_absolute_month(seg.start_month)
                                    gap_end_y, gap_end_m = from_absolute_month(seg.end_month)
                                    log(f"  Gap segment {gap_start_y}-{gap_start_m:02d} to {gap_end_y}-{gap_end_m:02d} "
                                        f"({seg.tob_code}) doesn't match prev pattern at {year}-{month:02d}: "
                                        f"{res_val:.3f} vs {pattern[month]:.3f}")
                                break
                        else:
                            pattern[month] = res_val

                    if matches and months_checked > 0:
                        prev_extended.append(gap_idx)
                    else:
                        break

                if len(prev_extended) > len(prev_region):
                    final_regions[-1] = prev_extended
                    if verbose:
                        log(f"  Extended previous region forward through gap")
                    # Remove extended indices from gap
                    gap_indices = [idx for idx in gap_indices if idx not in prev_extended]

                # Try to extend current region backward into remaining gap
                if gap_indices:
                    curr_extended = list(curr_region)
                    for gap_idx in reversed(gap_indices):
                        seg = segments[gap_idx]
                        seg_residuals = sorted(
                            ((abs_m, year, month, res_val) for (year, month), res_val in residuals.items()
                             if seg.start_month <= (abs_m := to_absolute_month(year, month)) <= seg.end_month),
                            reverse=True
                        )

                        # Build pattern from curr region
                        pattern = {}
                        for idx in curr_extended:
                            s = segments[idx]
                            for (year, month), res_val in residuals.items():
                                abs_month = to_absolute_month(year, month)
                                if s.start_month <= abs_month <= s.end_month:
                                    if month not in pattern:
                                        pattern[month] = res_val

                        # Check if gap segment matches pattern
                        matches = True
                        months_checked = 0
                        for abs_month, year, month, res_val in seg_residuals:
                            if month in pattern:
                                months_checked += 1
                                if abs(res_val - pattern[month]) > 0.01:
                                    matches = False
                                    if verbose:
                                        gap_start_y, gap_start_m = from_absolute_month(seg.start_month)
                                        gap_end_y, gap_end_m = from_absolute_month(seg.end_month)
                                        log(f"  Gap segment {gap_start_y}-{gap_start_m:02d} to {gap_end_y}-{gap_end_m:02d} "
                                            f"({seg.tob_code}) doesn't match curr pattern at {year}-{month:02d}: "
                                            f"{res_val:.3f} vs {pattern[month]:.3f}")
                                    break
                            else:
                                pattern[month] = res_val

                        if matches and months_checked > 0:
                            curr_extended.insert(0, gap_idx)
                        else:
                            break

                    if len(curr_extended) > len(curr_region):
                        curr_region = curr_extended
                        if verbose:
                            log(f"  Extended current region backward through gap")

            final_regions.append(curr_region)

        pathological_regions = final_regions

        # Process each pathological region independently to find best variance code
        pathological_replacements = {}  # Map region_indices tuple to merged segment

        for region_indices in pathological_regions:
            region_start = segments[region_indices[0]].start_month
            region_end = segments[region_indices[-1]].end_month

            # Find best variance code for THIS region
            best_code = None
            best_variance = float('inf')

            for code, basis in basis_vectors.items():
                diff_values = []
                for (year, month), res_val in residuals.items():
                    abs_month = to_absolute_month(year, month)
                    if region_start <= abs_month <= region_end:
                        if (year, month) in basis:
                            diff_values.append(res_val - basis[(year, month)])

                if len(diff_values) < MIN_SEGMENT_MONTHS:
                    continue

                mean = sum(diff_values) / len(diff_values)
                variance = sum((x - mean)**2 for x in diff_values) / len(diff_values)

                if variance < best_variance:
                    best_variance = variance
                    best_code = code

            if best_code is None:
                best_code = TOB_NO_BIAS_CODE

            if verbose:
                start_y, start_m = from_absolute_month(region_start)
                end_y, end_m = from_absolute_month(region_end)
                log(f"  Pathological region {start_y}-{start_m:02d} to {end_y}-{end_m:02d}: "
                    f"{best_code} (variance={best_variance:.6f})")

            merged_segment = TobSegment(
                start_month=region_start,
                end_month=region_end,
                tob_code=best_code,
                variance_score=best_variance,
                data_count=sum(segments[i].data_count for i in region_indices),
                include_in_his=True
            )
            pathological_replacements[tuple(region_indices)] = merged_segment

        # Handle gaps between pathological regions with same TOB code
        # These are PHA-only transitions that should be marked include_in_his=False
        gap_adjustments = {}  # Map segment index to adjusted segment
        for i in range(len(pathological_regions) - 1):
            curr_region_indices = pathological_regions[i]
            next_region_indices = pathological_regions[i + 1]

            curr_replacement = pathological_replacements[tuple(curr_region_indices)]
            next_replacement = pathological_replacements[tuple(next_region_indices)]

            # Check if same TOB code
            if curr_replacement.tob_code == next_replacement.tob_code:
                # Check for gap between regions
                gap_start_idx = curr_region_indices[-1] + 1
                gap_end_idx = next_region_indices[0] - 1

                if gap_start_idx <= gap_end_idx:
                    gap_start_month = segments[gap_start_idx].start_month
                    gap_end_month = segments[gap_end_idx].end_month
                    gap_months = gap_end_month - gap_start_month + 1

                    # If gap is short (< 12 months), mark as PHA-only transition
                    if gap_months < 12:
                        if verbose:
                            gap_start_y, gap_start_m = from_absolute_month(gap_start_month)
                            gap_end_y, gap_end_m = from_absolute_month(gap_end_month)
                            log(f"  Marking gap {gap_start_y}-{gap_start_m:02d} to {gap_end_y}-{gap_end_m:02d} "
                                f"as PHA-only transition (same code {curr_replacement.tob_code} both sides)")

                        for gap_idx in range(gap_start_idx, gap_end_idx + 1):
                            gap_seg = segments[gap_idx]
                            # Create adjusted segment with same code as adjacent regions, marked as PHA-only
                            adjusted_seg = TobSegment(
                                start_month=gap_seg.start_month,
                                end_month=gap_seg.end_month,
                                tob_code=curr_replacement.tob_code,  # Use same code as adjacent regions
                                variance_score=gap_seg.variance_score,
                                data_count=gap_seg.data_count,
                                include_in_his=False  # PHA-only, don't include in .his
                            )
                            gap_adjustments[gap_idx] = adjusted_seg

        # Build result: keep good segments, replace pathological regions
        all_pathological = set()
        for region_indices in pathological_regions:
            all_pathological.update(region_indices)

        fixed = []
        for i in range(len(segments)):
            if i not in all_pathological:
                # Check if this segment has gap adjustment
                if i in gap_adjustments:
                    fixed.append(gap_adjustments[i])
                else:
                    fixed.append(segments[i])
            else:
                # Check if this is the FIRST segment of a pathological region
                for region_indices in pathological_regions:
                    if region_indices[0] == i:
                        fixed.append(pathological_replacements[tuple(region_indices)])
                        break

        return fixed
    else:
        # No pathological pattern detected - return segments unchanged
        return segments


# ==============================================================================
# Phase 4.5: Boundary Polishing
# ==============================================================================

def polish_boundaries(segments: List[TobSegment],
                     residuals: Dict[Tuple[int, int], float],
                     basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
                     verbose: bool = False) -> List[TobSegment]:
    """Polish segment boundaries by adjusting ±1-2 months to minimize variance.

    This fixes cases where forward scan detected a break at month T, but the
    actual transition was at T±1 or T±2, creating single-month spikes.
    """
    if len(segments) <= 1:
        return segments

    polished = [segments[0]]  # First segment unchanged

    # Polish each boundary between adjacent segments
    for i in range(1, len(segments)):
        seg = segments[i]
        prev_seg = polished[-1]

        best_boundary = seg.start_month
        best_combined_var = prev_seg.variance_score + seg.variance_score

        # Try moving boundary ±1, ±2 months
        for shift in [-2, -1, 1, 2]:
            new_boundary = seg.start_month + shift

            # Must leave at least 6 months in each segment
            if new_boundary <= prev_seg.start_month + 6:
                continue
            if new_boundary >= seg.end_month - 6:
                continue

            # Calculate variance for prev segment with new end
            prev_var = calculate_segment_variance(
                residuals, basis_vectors[prev_seg.tob_code],
                prev_seg.start_month, new_boundary - 1
            )

            # Calculate variance for current segment with new start
            curr_var = calculate_segment_variance(
                residuals, basis_vectors[seg.tob_code],
                new_boundary, seg.end_month
            )

            combined_var = prev_var + curr_var

            if combined_var < best_combined_var:
                best_combined_var = combined_var
                best_boundary = new_boundary

        if best_boundary != seg.start_month:
            if verbose:
                old_y, old_m = from_absolute_month(seg.start_month)
                new_y, new_m = from_absolute_month(best_boundary)
                log(f"  Polished boundary: {old_y}-{old_m:02d} → {new_y}-{new_m:02d}")

            # Update previous segment end
            prev_seg_updated = TobSegment(
                start_month=prev_seg.start_month,
                end_month=best_boundary - 1,
                tob_code=prev_seg.tob_code,
                variance_score=calculate_segment_variance(
                    residuals, basis_vectors[prev_seg.tob_code],
                    prev_seg.start_month, best_boundary - 1
                ),
                data_count=prev_seg.data_count,
                include_in_his=prev_seg.include_in_his
            )
            polished[-1] = prev_seg_updated

            # Update current segment start
            seg = TobSegment(
                start_month=best_boundary,
                end_month=seg.end_month,
                tob_code=seg.tob_code,
                variance_score=calculate_segment_variance(
                    residuals, basis_vectors[seg.tob_code],
                    best_boundary, seg.end_month
                ),
                data_count=seg.data_count,
                include_in_his=seg.include_in_his
            )

        polished.append(seg)

    return polished


def calculate_segment_variance(residuals: Dict[Tuple[int, int], float],
                               basis: Dict[Tuple[int, int], float],
                               start_month: int, end_month: int) -> float:
    """Calculate variance for a segment with given code."""
    diff_values = []
    for (year, month), res_val in residuals.items():
        abs_month = to_absolute_month(year, month)
        if start_month <= abs_month <= end_month:
            if (year, month) in basis:
                diff_values.append(res_val - basis[(year, month)])

    if len(diff_values) < 2:
        return 0.0

    mean = sum(diff_values) / len(diff_values)
    variance = sum((x - mean)**2 for x in diff_values) / len(diff_values)
    return variance


# ==============================================================================
# Phase 5: Merge Same-Code Segments
# ==============================================================================

def merge_segments(segments: List[TobSegment], mshr_records: List[MshrRecord],
                   verbose: bool = False) -> List[TobSegment]:
    """Merge consecutive segments with the same TOB code if no MSHR records between them."""

    if len(segments) <= 1:
        return segments

    merged = []
    current_seg = segments[0]

    for next_seg in segments[1:]:
        if current_seg.tob_code == next_seg.tob_code:
            has_mshr_between = False
            for mshr in mshr_records:
                mshr_month = to_absolute_month(mshr.begin_date.year, mshr.begin_date.month)
                if current_seg.end_month < mshr_month <= next_seg.start_month:
                    has_mshr_between = True
                    break

            if not has_mshr_between:
                if verbose:
                    start_y, start_m = from_absolute_month(current_seg.start_month)
                    end_y, end_m = from_absolute_month(current_seg.end_month)
                    log(f"  Merging segments at {start_y}-{start_m:02d} to {end_y}-{end_m:02d}")

                current_seg = TobSegment(
                    start_month=current_seg.start_month,
                    end_month=next_seg.end_month,
                    tob_code=current_seg.tob_code,
                    variance_score=min(current_seg.variance_score, next_seg.variance_score),
                    include_in_his=current_seg.include_in_his or next_seg.include_in_his,
                    start_day=current_seg.start_day,  # Preserve day fields
                    end_day=next_seg.end_day
                )
                continue

        merged.append(current_seg)
        current_seg = next_seg

    merged.append(current_seg)

    return merged


# ==============================================================================
# Validation (copied from v2)
# ==============================================================================

def validate_segments(station_id: str, segments: List[TobSegment],
                     qcu_data: Dict[Tuple[int, int], int],
                     qcf_data: Dict[Tuple[int, int], int],
                     inv_entry: InventoryEntry,
                     tob_bin: Path,
                     qcu_file: Path,
                     verbose: bool = False,
                     debug_dir: Optional[Path] = None) -> bool:
    """Validate reconstructed TOB history and check for remaining exceedances."""

    if not segments:
        return True

    with tempfile.TemporaryDirectory() as temp_dir:
        work_dir = Path(temp_dir)
        his_file = work_dir / f"{station_id}.his"

        with open(his_file, 'w') as f:
            for seg in segments:
                start_y, start_m = from_absolute_month(seg.start_month)
                end_y, end_m = from_absolute_month(seg.end_month)

                lat_deg, lat_min, lat_sec = decimal_to_dms(inv_entry.lat)
                lon_deg, lon_min, lon_sec = decimal_to_dms(inv_entry.lon)
                elev = int(round(inv_entry.elev))

                begin_date = dt.date(start_y, start_m, 1)
                end_date = dt.date(end_y, end_m, 28)

                line = (
                    f"2"
                    f"{' ' * 12}"
                    f" {begin_date.year:4d}{begin_date.month:02d}{begin_date.day:02d}"
                    f" {end_date.year:4d}{end_date.month:02d}{end_date.day:02d}"
                    f" {lat_deg:3.0f}{lat_min:3.0f}{lat_sec:3.0f}"
                    f" {lon_deg:4.0f}{lon_min:3.0f}{lon_sec:3.0f}"
                    f" {' ' * 11}"
                    f" {elev:5d}"
                    f"  {' ' * 4}"
                    f" {seg.tob_code:4s}"
                    f"     {' ' * 66}"
                )
                f.write(line + "\n")

        tob_data = run_tobmain(tob_bin, station_id, qcu_file, his_file, work_dir, inv_entry, verbose, debug_dir)

        if tob_data is None:
            if verbose:
                log(f"  Validation failed: could not run TOBMain")
            return False

        val_residuals = {}
        for (year, month), qcf_val in qcf_data.items():
            if (year, month) in tob_data:
                tob_val = tob_data[(year, month)]
                val_residuals[(year, month)] = (qcf_val - tob_val) / VALUE_SCALE

        still_needs_tob = needs_tob_reconstruction(val_residuals)

        if verbose:
            if still_needs_tob:
                log(f"  Validation: FAILED - residual still shows TOB pattern")
            else:
                log(f"  Validation: PASSED - residual is clean (PHA-only)")

        return not still_needs_tob


# ==============================================================================
# Write output
# ==============================================================================

def write_his_file(station_id: str, segments: List[TobSegment],
                  inv_entry: InventoryEntry, out_dir: Path,
                  mshr_records: List[MshrRecord],
                  verbose: bool = False):
    """Write final .his file for a station.

    Only includes segments with include_in_his=True (TOB-related boundaries).
    """

    his_path = out_dir / f"{station_id}.his"

    # Filter segments to include only TOB-related boundaries
    his_segments = [seg for seg in segments if seg.include_in_his]

    with open(his_path, 'w') as f:
        for seg in his_segments:
            start_y, start_m = from_absolute_month(seg.start_month)
            end_y, end_m = from_absolute_month(seg.end_month)

            lat_deg, lat_min, lat_sec = decimal_to_dms(inv_entry.lat)
            lon_deg, lon_min, lon_sec = decimal_to_dms(inv_entry.lon)
            elev = int(round(inv_entry.elev))

            # Use start_day if specified, otherwise day 1
            start_day = seg.start_day if seg.start_day is not None else 1
            begin_date = dt.date(start_y, start_m, start_day)

            # Use end_day if specified, otherwise actual last day of month
            if seg.end_day is not None:
                end_day = seg.end_day
            else:
                _, end_day = calendar.monthrange(end_y, end_m)
            end_date = dt.date(end_y, end_m, end_day)

            line = (
                f"2"
                f"{' ' * 12}"
                f" {begin_date.year:4d}{begin_date.month:02d}{begin_date.day:02d}"
                f" {end_date.year:4d}{end_date.month:02d}{end_date.day:02d}"
                f" {lat_deg:3.0f}{lat_min:3.0f}{lat_sec:3.0f}"
                f" {lon_deg:4.0f}{lon_min:3.0f}{lon_sec:3.0f}"
                f" {' ' * 11}"
                f" {elev:5d}"
                f"  {' ' * 4}"
                f" {seg.tob_code:4s}"
                f"     {' ' * 66}"
            )
            f.write(line + "\n")

    if verbose:
        log(f"  Wrote {his_path} ({len(his_segments)} segments)")


# ==============================================================================
# Main execution
# ==============================================================================

def parse_args():
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description='Reconstruct TOB histories from QCU/QCF residuals using forward scanning'
    )
    parser.add_argument('--inv', required=True, help='Path to station.inv file')
    parser.add_argument('--qcu-dir', required=True, help='Directory containing QCU *.raw.tavg files')
    parser.add_argument('--qcf-dir', required=True, help='Directory containing QCF *.qcf.tavg files')
    parser.add_argument('--out-history-dir', required=True, help='Output directory for .his files')
    parser.add_argument('--mshr-zip', help='Path to mshr_enhanced.txt.zip (optional)')
    parser.add_argument('--tob-bin', required=True, help='Path to TOBMain binary')
    parser.add_argument('--test-stations', help='Comma-separated list of stations to test (optional)')
    parser.add_argument('--verbose', action='store_true', help='Enable verbose logging')
    parser.add_argument('--debug-dir', help='Directory to save failed TOBMain runs for debugging')
    return parser.parse_args()


def main():
    """Main entry point."""
    args = parse_args()

    tob_bin = Path(args.tob_bin).resolve()
    if not tob_bin.exists():
        log(f"ERROR: TOBMain binary not found: {tob_bin}")
        return 1

    log(f"Reading inventory from {args.inv}...")
    inventory = parse_inventory(Path(args.inv))
    log(f"  Loaded {len(inventory)} stations")

    if args.test_stations:
        test_set = set(s.strip() for s in args.test_stations.split(','))
        inventory = {sid: inv for sid, inv in inventory.items() if sid in test_set}
        log(f"  Testing with {len(inventory)} stations: {', '.join(sorted(inventory.keys()))}")

    mshr_records = {}
    if args.mshr_zip:
        log(f"Reading MSHR history from {args.mshr_zip}...")
        mshr_records = read_mshr_records(Path(args.mshr_zip), set(inventory.keys()))
        log(f"  Loaded MSHR records for {len(mshr_records)} stations")

    qcu_dir = Path(args.qcu_dir)
    qcf_dir = Path(args.qcf_dir)
    out_dir = Path(args.out_history_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    debug_dir = Path(args.debug_dir) if args.debug_dir else None
    if debug_dir:
        debug_dir.mkdir(parents=True, exist_ok=True)
        log(f"Debug mode: saving failed runs to {debug_dir}")

    stats = {
        'total': 0,
        'no_qcf': 0,
        'no_tob_needed': 0,
        'tob_needed': 0,
        'transitions_detected': 0,
        'validation_passed': 0,
        'validation_failed': 0,
        'failed_stations': [],  # Track which stations failed validation
    }

    total_stations = len(inventory)
    start_time = time.time()
    last_progress_report = start_time
    last_progress_pct = 0

    for station_id, inv_entry in sorted(inventory.items()):
        stats['total'] += 1

        current_time = time.time()
        elapsed = current_time - start_time
        current_pct = int((stats['total'] / total_stations) * 100)
        time_since_last_report = current_time - last_progress_report
        pct_since_last_report = current_pct - last_progress_pct

        should_report = (pct_since_last_report >= 10 or time_since_last_report >= 300)

        if should_report and stats['total'] > 1:
            avg_time_per_station = elapsed / stats['total']
            remaining_stations = total_stations - stats['total']
            eta_seconds = avg_time_per_station * remaining_stations

            elapsed_str = format_time(elapsed)
            eta_str = format_time(eta_seconds)

            log(f"Progress: {stats['total']}/{total_stations} ({current_pct}%) | "
                f"Elapsed: {elapsed_str} | ETA: {eta_str} | "
                f"No TOB: {stats['no_tob_needed']} | TOB recon: {stats['tob_needed']} | "
                f"Valid: {stats['validation_passed']}")

            last_progress_report = current_time
            last_progress_pct = current_pct

        if args.verbose:
            log(f"\n{'='*60}")
            log(f"Processing {station_id}...")

        qcu_file = qcu_dir / f"{station_id}.raw.tavg"
        qcf_file = qcf_dir / f"{station_id}.qcf.tavg"

        if not qcf_file.exists():
            stats['no_qcf'] += 1
            if args.verbose:
                log(f"  No QCF file, skipping")
            continue

        qcu_data = parse_station_data(qcu_file)
        qcf_data = parse_station_data(qcf_file)

        if args.verbose:
            log(f"  QCU: {len(qcu_data)} months")
            log(f"  QCF: {len(qcf_data)} months")

        residuals = compute_residuals(qcu_data, qcf_data)

        if args.verbose:
            log(f"  Residuals: {len(residuals)} months")

        if not needs_tob_reconstruction(residuals):
            stats['no_tob_needed'] += 1
            if args.verbose:
                log(f"  No TOB reconstruction needed (quick filter)")

            if residuals:
                sorted_times = sorted(residuals.keys())
                start_month = to_absolute_month(*sorted_times[0])
                end_month = to_absolute_month(*sorted_times[-1])
                segments = [TobSegment(start_month, end_month, TOB_NO_BIAS_CODE, 0.0)]
                write_his_file(station_id, segments, inv_entry, out_dir,
                             mshr_records.get(station_id, []), args.verbose)
            continue

        stats['tob_needed'] += 1
        if args.verbose:
            log(f"  TOB reconstruction needed")

        # Phase 1: Generate basis vectors
        basis_vectors = generate_basis_vectors(
            station_id, inv_entry, qcu_data, residuals,
            tob_bin, qcu_file, args.verbose, debug_dir
        )

        if not basis_vectors:
            log(f"  ERROR: Could not generate basis vectors for {station_id}")
            continue

        # Phase 2: Forward scanning
        segments = forward_scan(residuals, basis_vectors, args.verbose)

        if not segments:
            log(f"  ERROR: Forward scan produced no segments for {station_id}")
            continue

        # Phase 2.6: Validate and adjust boundary timing
        segments = validate_boundary_timing(segments, residuals, basis_vectors, args.verbose)

        # Phase 2.7: Refine spike boundaries with optimal day-of-month
        segments = refine_spike_boundaries(segments, residuals, basis_vectors, args.verbose)

        # Phase 2.8: Fix pathological segmentation (segments < 12 months)
        segments = detect_and_fix_pathological_segmentation(
            station_id, segments, residuals, basis_vectors, args.verbose
        )

        if len(segments) > 1:
            stats['transitions_detected'] += 1

        # Phase 3: Merge same-code segments
        station_mshr = mshr_records.get(station_id, [])
        segments = merge_segments(segments, station_mshr, args.verbose)

        # Validation
        is_valid = validate_segments(
            station_id, segments, qcu_data, qcf_data, inv_entry,
            tob_bin, qcu_file, args.verbose, debug_dir
        )

        if is_valid:
            stats['validation_passed'] += 1
        else:
            stats['validation_failed'] += 1
            stats['failed_stations'].append(station_id)
            if args.verbose:
                log(f"  WARNING: Validation failed, but writing segments anyway")

        # Write output (only TOB-related segments with include_in_his=True)
        write_his_file(station_id, segments, inv_entry, out_dir,
                      mshr_records.get(station_id, []), args.verbose)

    total_elapsed = time.time() - start_time
    log(f"\n{'='*60}")
    log("FINAL SUMMARY")
    log(f"{'='*60}")
    log(f"Total time: {format_time(total_elapsed)}")
    log(f"Average: {format_time(total_elapsed / stats['total']) if stats['total'] > 0 else '0s'} per station")
    log(f"")
    log(f"  Total stations processed: {stats['total']}")
    log(f"  No QCF file:              {stats['no_qcf']}")
    log(f"  No TOB needed:            {stats['no_tob_needed']}")
    log(f"  TOB reconstruction:       {stats['tob_needed']}")
    log(f"  Transitions detected:     {stats['transitions_detected']}")
    log(f"  Validation passed:        {stats['validation_passed']}")
    log(f"  Validation failed:        {stats['validation_failed']}")

    if stats['failed_stations']:
        log(f"")
        log(f"Failed stations ({len(stats['failed_stations'])}):")
        for station_id in sorted(stats['failed_stations']):
            log(f"  {station_id}")

    log(f"{'='*60}")

    return 0


if __name__ == '__main__':
    sys.exit(main())
