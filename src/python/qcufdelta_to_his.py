#!/usr/bin/env python3
"""Reconstruct Time-of-Observation histories from USHCN QCU/QCF residuals.

This implements the forward scanning algorithm:
- Phase 1: Basis vector generation
- Phase 2: Forward scanning with gap handling (≥12 months)
- Phase 3: Boundary timing validation (adjust ±6 months for perfect fit)
- Phase 3b: Back-attribute trailing months at A→B boundary to B's regime
- Phase 4: Spike boundary refinement (day-level transition detection)
- Phase 4b: Multi-month edge anomaly healing at A→B boundaries
- Phase 5: Pathological segmentation detection and repair
- Phase 6: Short bridge segment recoding to enable merging
- Phase 7: Cross-gap TOB attribution (PHA-continuity test)
- Phase 8: Merge same-code segments (with MSHR awareness)
- Phase 8.5: Trim anomalous leading months from first segment (24HR prefix)
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
    lat: Optional[float]    # LAT_DEC decimal degrees; None if missing
    lon: Optional[float]    # LON_DEC decimal degrees; None if missing
    elev_ft: Optional[int]  # ELEV_GROUND ground elevation in feet; None if missing
    relocation: str         # RELOCATION field (62 chars); non-blank = station moved at begin_date


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


def _active_mshr_at(date: dt.date, mshr_records: List['MshrRecord']) -> Optional['MshrRecord']:
    """Return the MSHR record with the latest begin_date <= date, or None.

    Assumes mshr_records is sorted ascending by begin_date.
    """
    result = None
    for m in mshr_records:
        if m.begin_date <= date:
            result = m
        else:
            break
    return result


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

                # MSHR Enhanced Table columns (1-indexed in spec, 0-indexed here):
                #   LAT_DEC      1300-1319  → [1299:1319]
                #   LON_DEC      1321-1340  → [1320:1340]
                #   ELEV_GROUND   990-1029  → [ 989:1029]  (in feet)
                #   RELOCATION   1353-1414  → [1352:1414]
                try:
                    lat_raw = line[1299:1319].strip()
                    lat = float(lat_raw) if lat_raw else None
                except ValueError:
                    lat = None
                try:
                    lon_raw = line[1320:1340].strip()
                    lon = float(lon_raw) if lon_raw else None
                except ValueError:
                    lon = None
                try:
                    elev_raw = line[989:1029].strip()
                    elev_ft = int(round(float(elev_raw))) if elev_raw else None
                except ValueError:
                    elev_ft = None

                relocation = line[1352:1414]

                rec = MshrRecord(
                    station_id=station_id,
                    begin_date=begin_date,
                    end_date=end_date,
                    lat=lat,
                    lon=lon,
                    elev_ft=elev_ft,
                    relocation=relocation,
                )
                records[station_id].append(rec)

    for station_id in records:
        records[station_id] = sorted(records[station_id],
                                    key=lambda r: (r.begin_date, r.end_date))

    return records


# ==============================================================================
# Data Preparation
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
# Phase 1: Basis Vector Generation
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


def generate_mshr_aware_his(station_id: str, tob_code: str,
                            record_start: Tuple[int, int], record_end: Tuple[int, int],
                            mshr_records: List[MshrRecord],
                            inv_entry: InventoryEntry) -> str:
    """Generate a .his with a single TOB code but MSHR-derived lat/lon per period.

    Creates one row per MSHR location period within the record range so that
    TOBMain (run with tob.use-his-lat-lon=true) uses the correct lat/lon for
    each sub-period when computing the expected adjustment.
    """
    begin_data = dt.date(record_start[0], record_start[1], 1)
    end_data = dt.date(record_end[0], record_end[1], 28)
    inv_elev_ft = int(round(inv_entry.elev * 3.28084))

    # Collect period start-dates: record start plus any MSHR begin_date within record
    boundaries: List[dt.date] = [begin_data]
    for mshr in mshr_records:
        if begin_data < mshr.begin_date <= end_data:
            boundaries.append(mshr.begin_date)
    boundaries.sort()

    lines = []
    for i, bdate in enumerate(boundaries):
        edate = boundaries[i + 1] - dt.timedelta(days=1) if i < len(boundaries) - 1 else end_data

        active = _active_mshr_at(bdate, mshr_records)
        if active and active.lat is not None and active.lon is not None:
            lat_deg, lat_min, lat_sec = decimal_to_dms(active.lat)
            lon_deg, lon_min, lon_sec = decimal_to_dms(active.lon)
        else:
            lat_deg, lat_min, lat_sec = decimal_to_dms(inv_entry.lat)
            lon_deg, lon_min, lon_sec = decimal_to_dms(inv_entry.lon)
        elev = active.elev_ft if (active and active.elev_ft is not None) else inv_elev_ft

        lines.append(
            f"2"
            f"{' ' * 12}"
            f" {bdate.year:4d}{bdate.month:02d}{bdate.day:02d}"
            f" {edate.year:4d}{edate.month:02d}{edate.day:02d}"
            f" {lat_deg:3.0f}{lat_min:3.0f}{lat_sec:3.0f}"
            f" {lon_deg:4.0f}{lon_min:3.0f}{lon_sec:3.0f}"
            f" {' ' * 11}"
            f" {elev:5d}"
            f"  {' ' * 4}"
            f" {tob_code:4s}"
            f"     {' ' * 66}"
        )
    return "\n".join(lines)


def run_tobmain(tob_bin: Path, station_id: str, qcu_file: Path, his_file: Path,
               work_dir: Path, inv_entry: InventoryEntry,
               use_mshr_lat_lon: bool = False,
               verbose: bool = False,
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

    use_his_lat_lon_str = "true" if use_mshr_lat_lon else "false"
    props_file = work_dir / "tob.properties"
    with open(props_file, 'w') as f:
        f.write(f"""pha.element = tavg
pha.path.station-metadata = {inv_file}
pha.path.station-history = {hist_dir}/

tob.start-year = 1700
tob.start-from-history = true
tob.use-his-lat-lon = {use_his_lat_lon_str}
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
                          mshr_records: List[MshrRecord],
                          use_mshr_lat_lon: bool = False,
                          verbose: bool = False,
                          debug_dir: Optional[Path] = None) -> Dict[str, Dict[Tuple[int, int], float]]:
    """Generate basis vectors for all TOB codes.

    Each synthetic .his encodes the MSHR location history (when use_mshr_lat_lon
    is True) so that TOBMain computes the expected adjustment using the correct
    lat/lon for each sub-period.  Falls back to the inventory lat/lon for
    periods with no MSHR record or missing coordinates.
    """

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

            # Create .his file with this TOB code.
            # With MSHR lat/lon: multi-period .his so TOBMain uses per-location adjustments.
            # Without: single-period .his with the inventory lat/lon.
            if use_mshr_lat_lon:
                his_content = generate_mshr_aware_his(
                    fake_id, code, record_start, record_end, mshr_records, inv_entry
                )
            else:
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

        use_his_lat_lon_str = "true" if use_mshr_lat_lon else "false"
        # Run TOBMain ONCE for all 30 synthetic stations
        props_file = work_dir / "tob.properties"
        with open(props_file, 'w') as f:
            f.write(f"""pha.element = tavg
pha.path.station-metadata = {inv_file}
pha.path.station-history = {hist_dir}/

tob.start-year = 1700
tob.start-from-history = true
tob.use-his-lat-lon = {use_his_lat_lon_str}
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
# Boundary Refinement Helpers
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
    it ±1-6 months would result in both adjacent segments having perfect fit
    (≤3 distinct values). This corrects for cases where the boundary refinement
    didn't find the exact transition month — in particular when a missing data
    month immediately before the true transition causes the forward scan to land
    several months late.

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

        # Try adjusting boundary ±1–6 months to minimize total distinct values.
        # Even if both segments have "perfect fit" (≤3 distinct), we should prefer
        # fewer total distinct values (e.g., 1+3=4 is better than 3+3=6).
        # ±6 is needed because a data gap (missing month) immediately before the
        # true transition can cause the forward scan to land several months late.
        best_boundary = seg.start_month
        best_prev_distinct = prev_distinct
        best_curr_distinct = curr_distinct
        best_total = prev_distinct + curr_distinct

        # Iterate in order of increasing absolute magnitude so that, when two
        # shifts produce identical total distinct counts, the smaller (closer to
        # zero) shift is preferred.  Larger shifts still win when they strictly
        # reduce the total (e.g. a missing-data month forces a 5-month correction).
        for shift in [-1, 1, -2, 2, -3, 3, -4, 4, -5, 5, -6, 6]:
            new_boundary = seg.start_month + shift

            # Must leave enough months in each segment after the shift.
            # For short segments (e.g. the first post-gap window), scale the
            # minimum down so that boundary shifts are not completely blocked.
            prev_span = prev_seg.end_month - prev_seg.start_month
            curr_span = seg.end_month - seg.start_month
            prev_min = min(MIN_SEGMENT_MONTHS, max(2, prev_span // 2))
            curr_min = min(MIN_SEGMENT_MONTHS, max(2, curr_span // 2))
            if new_boundary <= prev_seg.start_month + prev_min:
                continue
            if new_boundary >= seg.end_month - curr_min:
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
            # 2. Total distinct values decreases, OR
            # 3. Total is tied but the old (prev) segment becomes cleaner.
            #    A cleaner old segment means the established regime ends at a
            #    well-defined point.  This also allows Phase 4 to trigger on
            #    partial-month transitions that sit in the new segment.
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
            elif new_total == best_total and new_prev_distinct < best_prev_distinct:
                # Tied total but old segment is cleaner
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


def back_attribute_trailing_months(
        segments: List[TobSegment],
        residuals: Dict[Tuple[int, int], float],
        basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
        verbose: bool = False) -> List[TobSegment]:
    """Phase 3b: Move trailing months from segment A to B at each A→B boundary.

    When the last few months of segment A have adjusted residuals (R - S_B) that
    already belong to segment B's stable value set, those months are operating at
    B's TOB regime even though the forward scan placed them in A.  Shift the
    boundary earlier by moving those months into B.

    This corrects cases where NOAA's PHA step lands a few months before the
    boundary detected by the forward scan, causing a short run at A's end to
    have B-level residuals.

    Conditions to move the last k months of A to B:
      - All k months have adj_B (R - S_B) values already in B's adj set.
      - Removing those k months keeps A's distinct count ≤ 3.
      - At least MIN_SEGMENT_MONTHS data months remain in A.
    """
    if len(segments) <= 1:
        return segments

    MAX_BACK = 6

    result = list(segments)

    for i in range(1, len(result)):
        prev = result[i - 1]
        curr = result[i]

        if not prev.include_in_his or not curr.include_in_his:
            continue
        if prev.tob_code == curr.tob_code:
            continue
        if prev.tob_code not in basis_vectors or curr.tob_code not in basis_vectors:
            continue

        basis_prev = basis_vectors[prev.tob_code]
        basis_curr = basis_vectors[curr.tob_code]

        # All data months in prev segment (sorted ascending)
        all_prev_months = sorted(
            to_absolute_month(y, m) for (y, m) in residuals
            if prev.start_month <= to_absolute_month(y, m) <= prev.end_month
            and (y, m) in basis_prev
        )
        if len(all_prev_months) <= MIN_SEGMENT_MONTHS:
            continue

        # B's (curr segment) stable adj value set.
        # Use only months at offset ≥7 from B's start so that any anomalous
        # leading months of B (e.g. a partial transition month) don't pollute
        # the set and cause spurious back-attribution of A's tail months.
        B_adj_set = set()
        for (y, m) in residuals:
            am = to_absolute_month(y, m)
            if am - curr.start_month >= 7 and am <= curr.end_month and (y, m) in basis_curr:
                B_adj_set.add(int(round((residuals[(y, m)] - basis_curr[(y, m)]) * 100)))
        if not B_adj_set:
            continue

        # Walk backward through A's tail months
        max_k = min(MAX_BACK, len(all_prev_months) - MIN_SEGMENT_MONTHS)
        if max_k <= 0:
            continue

        best_k = 0
        for k in range(1, max_k + 1):
            tail_am = all_prev_months[-k]
            ty, tm = from_absolute_month(tail_am)

            if (ty, tm) not in basis_curr:
                break  # Can't compute adj_B for this month

            adj_B = int(round((residuals[(ty, tm)] - basis_curr[(ty, tm)]) * 100))
            if adj_B not in B_adj_set:
                break  # Month doesn't match B's stable level

            # Check remaining A data (all months strictly before tail_am)
            remaining = [am for am in all_prev_months if am < tail_am]
            if len(remaining) < MIN_SEGMENT_MONTHS:
                break

            A_remaining_adj = set()
            for ram in remaining:
                ry, rm = from_absolute_month(ram)
                A_remaining_adj.add(
                    int(round((residuals[(ry, rm)] - basis_prev[(ry, rm)]) * 100))
                )
            if len(A_remaining_adj) <= 3:
                best_k = k

        if best_k > 0:
            tail_am = all_prev_months[-best_k]
            new_prev_end = tail_am - 1  # A ends one calendar month before first moved month

            if verbose:
                old_b_y, old_b_m = from_absolute_month(curr.start_month)
                new_b_y, new_b_m = from_absolute_month(tail_am)
                log(f"  Back-attributed {best_k} months: "
                    f"{prev.tob_code}→{curr.tob_code} boundary "
                    f"{old_b_y}-{old_b_m:02d} → {new_b_y}-{new_b_m:02d}")

            result[i - 1] = TobSegment(
                start_month=prev.start_month,
                end_month=new_prev_end,
                tob_code=prev.tob_code,
                variance_score=prev.variance_score,
                data_count=prev.data_count,
                include_in_his=prev.include_in_his,
                start_day=prev.start_day,
                end_day=prev.end_day,
            )
            result[i] = TobSegment(
                start_month=tail_am,
                end_month=curr.end_month,
                tob_code=curr.tob_code,
                variance_score=curr.variance_score,
                data_count=curr.data_count,
                include_in_his=curr.include_in_his,
                start_day=curr.start_day,
                end_day=curr.end_day,
            )

    return result


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

        TOLERANCE = 0.01

        # ---- BACKWARD SPIKE CHECK ----
        # Check if the LAST month of the previous segment is the true transition
        # month (i.e. the regime change happened mid-month within prev_seg.end_month
        # rather than at the start of seg.start_month).
        back_y, back_m = from_absolute_month(prev_seg.end_month)
        if ((back_y, back_m) in residuals
                and (back_y, back_m) in basis_vectors.get(prev_seg.tob_code, {})
                and (back_y, back_m) in basis_vectors.get(seg.tob_code, {})):
            back_residual = residuals[(back_y, back_m)]
            diff_prev_b = back_residual - basis_vectors[prev_seg.tob_code][(back_y, back_m)]
            diff_curr_b = back_residual - basis_vectors[seg.tob_code][(back_y, back_m)]

            ref_month_b = prev_seg.end_month
            prev_diffs_b = []
            curr_diffs_b = []
            for offset in range(-6, 7):
                if offset == 0:
                    continue
                cm_b = ref_month_b + offset
                cy_b, cmm_b = from_absolute_month(cm_b)
                if (cy_b, cmm_b) in residuals:
                    if offset < 0 and (cy_b, cmm_b) in basis_vectors.get(prev_seg.tob_code, {}):
                        prev_diffs_b.append(
                            residuals[(cy_b, cmm_b)] - basis_vectors[prev_seg.tob_code][(cy_b, cmm_b)]
                        )
                    if offset > 0 and (cy_b, cmm_b) in basis_vectors.get(seg.tob_code, {}):
                        curr_diffs_b.append(
                            residuals[(cy_b, cmm_b)] - basis_vectors[seg.tob_code][(cy_b, cmm_b)]
                        )

            if prev_diffs_b and curr_diffs_b:
                prev_min_b, prev_max_b = min(prev_diffs_b), max(prev_diffs_b)
                curr_min_b, curr_max_b = min(curr_diffs_b), max(curr_diffs_b)
                prev_out_b = (diff_prev_b < prev_min_b - TOLERANCE
                              or diff_prev_b > prev_max_b + TOLERANCE)
                curr_out_b = (diff_curr_b < curr_min_b - TOLERANCE
                              or diff_curr_b > curr_max_b + TOLERANCE)

                if prev_out_b and curr_out_b:
                    _, days_b = calendar.monthrange(back_y, back_m)
                    midpoint_b = (prev_min_b + prev_max_b + curr_min_b + curr_max_b) / 4.0
                    best_day_b = None
                    best_distinct_b = float('inf')
                    best_in_stable_b = False
                    best_mid_b = float('inf')

                    for split_day_b in range(2, days_b + 1):
                        days_old = split_day_b - 1
                        days_new = days_b - split_day_b + 1
                        weighted_diff_b = (
                            (days_old * diff_prev_b + days_new * diff_curr_b) / days_b
                        )
                        wc_b = int(round(weighted_diff_b * 100))
                        nearby_b = {wc_b}
                        for offset in range(-3, 4):
                            if offset == 0:
                                continue
                            cm2_b = ref_month_b + offset
                            cy2_b, cmm2_b = from_absolute_month(cm2_b)
                            if (cy2_b, cmm2_b) in residuals:
                                basis_b = (basis_vectors[seg.tob_code]
                                           if offset > 0
                                           else basis_vectors[prev_seg.tob_code])
                                if (cy2_b, cmm2_b) in basis_b:
                                    nd_b = (residuals[(cy2_b, cmm2_b)]
                                            - basis_b[(cy2_b, cmm2_b)])
                                    nearby_b.add(int(round(nd_b * 100)))
                        dc_b = len(nearby_b)
                        md_b = abs(weighted_diff_b - midpoint_b)
                        in_stable_b = (
                            prev_min_b - TOLERANCE <= weighted_diff_b <= prev_max_b + TOLERANCE or
                            curr_min_b - TOLERANCE <= weighted_diff_b <= curr_max_b + TOLERANCE
                        )
                        if (dc_b < best_distinct_b or
                                (dc_b == best_distinct_b and in_stable_b and not best_in_stable_b) or
                                (dc_b == best_distinct_b and in_stable_b == best_in_stable_b
                                 and md_b < best_mid_b)):
                            best_distinct_b = dc_b
                            best_in_stable_b = in_stable_b
                            best_mid_b = md_b
                            best_day_b = split_day_b

                    if best_day_b is not None and best_day_b > 1:
                        if verbose:
                            log(f"  Refined backward spike at {back_y}-{back_m:02d} "
                                f"split day {best_day_b} "
                                f"(spike: {diff_prev_b:+.3f}/{diff_curr_b:+.3f}°C, "
                                f"distinct: {best_distinct_b})")
                        prev_seg_updated_b = TobSegment(
                            start_month=prev_seg.start_month,
                            end_month=prev_seg.end_month,
                            tob_code=prev_seg.tob_code,
                            variance_score=prev_seg.variance_score,
                            data_count=prev_seg.data_count,
                            include_in_his=prev_seg.include_in_his,
                            start_day=prev_seg.start_day,
                            end_day=best_day_b - 1,
                        )
                        refined[-1] = prev_seg_updated_b
                        seg_updated_b = TobSegment(
                            start_month=prev_seg.end_month,  # share the transition month
                            end_month=seg.end_month,
                            tob_code=seg.tob_code,
                            variance_score=seg.variance_score,
                            data_count=seg.data_count,
                            include_in_his=seg.include_in_his,
                            start_day=best_day_b,
                            end_day=seg.end_day,
                        )
                        refined.append(seg_updated_b)
                        continue  # backward fix applied; skip forward check

        # ---- FORWARD SPIKE CHECK ----
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

        # Get residual ranges from nearby months.
        # Deliberately one-sided: prev_diffs from the OLD segment only (offset<0),
        # curr_diffs from the NEW segment only (offset>0).  Using both sides for
        # each code would contaminate the range with months where that code is no
        # longer active, artificially widening the "in-bound" interval.
        prev_diffs = []
        curr_diffs = []
        for offset in range(-6, 7):
            if offset == 0:
                continue  # Skip transition month itself
            check_month = seg.start_month + offset
            check_y, check_m = from_absolute_month(check_month)
            if (check_y, check_m) in residuals:
                if offset < 0 and (check_y, check_m) in basis_vectors.get(prev_seg.tob_code, {}):
                    prev_diffs.append(residuals[(check_y, check_m)] - basis_vectors[prev_seg.tob_code][(check_y, check_m)])
                if offset > 0 and (check_y, check_m) in basis_vectors.get(seg.tob_code, {}):
                    curr_diffs.append(residuals[(check_y, check_m)] - basis_vectors[seg.tob_code][(check_y, check_m)])

        if not prev_diffs or not curr_diffs:
            refined.append(seg)
            continue

        # Check if transition month residual is out of bounds
        prev_min, prev_max = min(prev_diffs), max(prev_diffs)
        curr_min, curr_max = min(curr_diffs), max(curr_diffs)

        # Out of bound check (±0.01°C tolerance)
        prev_out_of_bound = (diff_prev < prev_min - TOLERANCE or diff_prev > prev_max + TOLERANCE)
        curr_out_of_bound = (diff_curr < curr_min - TOLERANCE or diff_curr > curr_max + TOLERANCE)

        # Require at least the new-code residual to be anomalous.
        if not curr_out_of_bound:
            refined.append(seg)
            continue

        # When only the new code is out of bound (prev is within its range), the seasonal
        # spread of the old code may contain the transition month (e.g. winter vs summer
        # months of the same old-code segment straddle the transition month's diff).
        # Allow the split if the anomaly on the new code is large enough to be credible.
        BOUNDARY_MONTH_MIN_ANOMALY = 0.05  # ≥ 5 centidegrees required for one-sided detection
        if not prev_out_of_bound:
            curr_anomaly = max(
                diff_curr - (curr_max + TOLERANCE),
                (curr_min - TOLERANCE) - diff_curr,
            )
            if curr_anomaly < BOUNDARY_MONTH_MIN_ANOMALY:
                refined.append(seg)
                continue

        # Search for optimal day split
        _, days_in_month = calendar.monthrange(transition_y, transition_m)

        best_day = None
        best_distinct_count = float('inf')
        best_in_stable = False
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

            # True if weighted blend falls within one of the two stable level ranges.
            # Prefer these days: they assign the transition month cleanly to one level.
            in_stable = (
                prev_min - TOLERANCE <= weighted_diff <= prev_max + TOLERANCE or
                curr_min - TOLERANCE <= weighted_diff <= curr_max + TOLERANCE
            )

            # Priority: fewest distinct values, then in stable range, then midpoint distance
            if (distinct_count < best_distinct_count or
                    (distinct_count == best_distinct_count and in_stable and not best_in_stable) or
                    (distinct_count == best_distinct_count and in_stable == best_in_stable
                     and midpoint_distance < best_midpoint_distance)):
                best_distinct_count = distinct_count
                best_in_stable = in_stable
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


def heal_edge_transitions(
        segments: List[TobSegment],
        residuals: Dict[Tuple[int, int], float],
        basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
        verbose: bool = False) -> List[TobSegment]:
    """Phase 4b: Heal multi-month anomalies at code-changing boundaries.

    At an A→B boundary the first few months of B sometimes show a residual
    (R - S_B) outside the stable B range because NOAA's QCF placed those months
    in the adjacent PHA step level.  When months 0..K-2 of B land on the stable
    B adj under code A instead, we extend the A segment through those months and
    day-split the last anomalous month (month K-1) at the day that brings its
    blended adj into the stable B range.

    Criterion: K ≤ MAX_EDGE anomalous leading months, each month 0..K-2 gives
    adj_A in the stable B range, and a valid split day exists for month K-1.
    """
    MAX_EDGE = 4
    TOLERANCE_CENTS = 1   # ±1 centidegree (= 0.01°C)
    MIN_STABLE_MONTHS = 3

    result = list(segments)
    for i in range(1, len(result)):
        prev = result[i - 1]
        curr = result[i]
        if not prev.include_in_his or not curr.include_in_his:
            continue
        if prev.tob_code == curr.tob_code:
            continue
        if prev.tob_code not in basis_vectors or curr.tob_code not in basis_vectors:
            continue

        basis_prev = basis_vectors[prev.tob_code]
        basis_curr = basis_vectors[curr.tob_code]

        # Stable B adj range: months at offsets +7..+18 from B-segment start
        stable_adjs = []
        for offset in range(7, 19):
            am = curr.start_month + offset
            if am > curr.end_month:
                break
            y, m = from_absolute_month(am)
            if (y, m) in residuals and (y, m) in basis_curr:
                stable_adjs.append(
                    int(round((residuals[(y, m)] - basis_curr[(y, m)]) * 100))
                )
        if len(stable_adjs) < MIN_STABLE_MONTHS:
            continue
        stable_min = min(stable_adjs)
        stable_max = max(stable_adjs)

        def in_stable(adj_cents: int) -> bool:
            return stable_min - TOLERANCE_CENTS <= adj_cents <= stable_max + TOLERANCE_CENTS

        # Find anomalous leading months of B (adj_B outside stable range)
        first_months: List[Tuple[int, int]] = []   # (abs_month, adj_B_cents)
        for k in range(MAX_EDGE + 1):
            am = curr.start_month + k
            if am > curr.end_month:
                break
            y, m = from_absolute_month(am)
            if (y, m) not in residuals or (y, m) not in basis_curr:
                break
            adj_B = int(round((residuals[(y, m)] - basis_curr[(y, m)]) * 100))
            if not in_stable(adj_B):
                first_months.append((am, adj_B))
            else:
                break

        K = len(first_months)
        # K=0: no anomaly.  K=1: already handled by Phase 4 spike refinement.
        # K>MAX_EDGE: too many anomalous months, skip.
        if K < 2 or K > MAX_EDGE:
            continue

        # Months 0..K-2 must land on the stable B adj under code A
        ok = True
        for k in range(K - 1):
            am, _ = first_months[k]
            y, m = from_absolute_month(am)
            if (y, m) not in basis_prev:
                ok = False
                break
            adj_A = int(round((residuals[(y, m)] - basis_prev[(y, m)]) * 100))
            if not in_stable(adj_A):
                ok = False
                break
        if not ok:
            continue

        # Find optimal split day within month K-1 (M_last)
        m_last_am, _ = first_months[K - 1]
        my, mm = from_absolute_month(m_last_am)
        if (my, mm) not in residuals or (my, mm) not in basis_prev or (my, mm) not in basis_curr:
            continue

        R_last = residuals[(my, mm)]
        S_A_last = basis_prev[(my, mm)]
        S_B_last = basis_curr[(my, mm)]
        diff_A = R_last - S_A_last   # adj under pure A (°C)
        diff_B = R_last - S_B_last   # adj under pure B (°C)

        _, days_in_month = calendar.monthrange(my, mm)
        stable_center = (stable_min + stable_max) / 200.0  # °C

        best_day = None
        best_dist = float('inf')
        for d in range(1, days_in_month + 1):
            days_A = d - 1
            days_B = days_in_month - d + 1
            blended = (days_A * diff_A + days_B * diff_B) / days_in_month
            adj_blended = int(round(blended * 100))
            if in_stable(adj_blended):
                dist = abs(blended - stable_center)
                if dist < best_dist:
                    best_dist = dist
                    best_day = d

        if best_day is None or best_day == 1:
            continue

        if verbose:
            old_by, old_bm = from_absolute_month(curr.start_month)
            anomalous = [a for _, a in first_months]
            log(f"  Healed {K}-month edge anomaly: {prev.tob_code}→{curr.tob_code} "
                f"boundary moved from {old_by}-{old_bm:02d} "
                f"to {my}-{mm:02d}-{best_day:02d} "
                f"(stable [{stable_min},{stable_max}], anomalous adj: {anomalous})")

        result[i - 1] = TobSegment(
            start_month=prev.start_month, end_month=m_last_am,
            tob_code=prev.tob_code, variance_score=prev.variance_score,
            data_count=prev.data_count, include_in_his=prev.include_in_his,
            start_day=prev.start_day, end_day=best_day - 1,
        )
        result[i] = TobSegment(
            start_month=m_last_am, end_month=curr.end_month,
            tob_code=curr.tob_code, variance_score=curr.variance_score,
            data_count=curr.data_count, include_in_his=curr.include_in_his,
            start_day=best_day, end_day=curr.end_day,
        )

    return result


# ==============================================================================
# Code Selection (Tie-Breaking)
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
# Walk-Back for TOB+PHA Cases
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
# Phase 5: Pathological Segmentation Detection and Repair
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

        # Merge any overlapping final regions (can occur when two grown regions
        # have consumed the same segment indices from different seed points).
        merged_final = []
        for region_indices in final_regions:
            region_set = set(region_indices)
            if not merged_final:
                merged_final.append(sorted(region_set))
                continue
            # Regions are sorted by start index; overlap iff the new region's
            # first index falls at or before the previous region's last index.
            if region_indices[0] <= merged_final[-1][-1]:
                merged_final[-1] = sorted(set(merged_final[-1]) | region_set)
                if verbose:
                    log(f"  Merged overlapping pathological regions")
            else:
                merged_final.append(sorted(region_set))
        pathological_regions = merged_final

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
# Boundary Polishing
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
# Phase 6: Short Bridge Segment Recoding
# ==============================================================================

def recode_short_segments(segments: List[TobSegment],
                          residuals: Dict[Tuple[int, int], float],
                          basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
                          verbose: bool = False) -> List[TobSegment]:
    """Recode segments with fewer than MIN_SEGMENT_MONTHS data points.

    A short "bridge" segment (e.g. 6-month 24HR between 07HR and 00SS) can arise
    when the forward scan crosses a gap.  The segment may be assigned a code that
    fits its few data points no better than either neighbour's code would.  If a
    neighbour's code gives strictly fewer distinct values (or equal distinct values
    but the neighbour's code is the same on both sides), re-assign this segment
    to that code so that Phase 8 (merge_segments) will fold it away.

    Only recodes include_in_his segments; excluded-from-his segments are left alone.
    Only considers the codes of immediately adjacent segments as candidates.
    """
    if len(segments) <= 1:
        return segments

    result = list(segments)
    changed = True
    while changed:
        changed = False
        for i, seg in enumerate(result):
            if not seg.include_in_his:
                continue

            # Use effective boundaries: clamp to adjacent segments' extents.
            # Phase 5 can produce overlapping segments (a pathological region
            # whose nominal end_month falls inside the next segment's range).
            # The write_his_file output clips such segments at the next start,
            # so we must assess the segment on its *effective* data window.
            eff_start = seg.start_month
            eff_end = seg.end_month
            if i > 0 and result[i - 1].end_month >= eff_start:
                eff_start = result[i - 1].end_month + 1
            if i < len(result) - 1 and result[i + 1].start_month <= eff_end:
                eff_end = result[i + 1].start_month - 1

            if eff_start > eff_end:
                continue  # fully subsumed — nothing to recode

            # Count data points inside the effective window
            eff_count = sum(
                1 for (year, month) in residuals
                if eff_start <= to_absolute_month(year, month) <= eff_end
            )
            if eff_count >= MIN_SEGMENT_MONTHS:
                continue

            # Gather candidate codes from adjacent segments
            candidates = {}
            if i > 0 and result[i - 1].include_in_his:
                prev_code = result[i - 1].tob_code
                if prev_code in basis_vectors:
                    candidates[prev_code] = count_segment_distinct_values(
                        residuals, basis_vectors[prev_code],
                        eff_start, eff_end
                    )
            if i < len(result) - 1 and result[i + 1].include_in_his:
                next_code = result[i + 1].tob_code
                if next_code in basis_vectors:
                    candidates[next_code] = count_segment_distinct_values(
                        residuals, basis_vectors[next_code],
                        eff_start, eff_end
                    )

            if not candidates:
                continue

            # Current distinct-value count with existing code
            current_distinct = (
                count_segment_distinct_values(
                    residuals, basis_vectors[seg.tob_code],
                    eff_start, eff_end
                ) if seg.tob_code in basis_vectors else 999
            )

            # Pick the best candidate: fewest distinct values; break ties by
            # preferring the code that appears on both sides (so both neighbours
            # merge), then by preferring the previous-segment code.
            best_code = None
            best_distinct = current_distinct
            both_sides_code = None
            if (i > 0 and i < len(result) - 1
                    and result[i - 1].include_in_his
                    and result[i + 1].include_in_his
                    and result[i - 1].tob_code == result[i + 1].tob_code):
                both_sides_code = result[i - 1].tob_code

            for code, distinct in candidates.items():
                if distinct < best_distinct:
                    best_code = code
                    best_distinct = distinct
                elif distinct == best_distinct and code == both_sides_code:
                    best_code = code  # prefer code that appears on both sides

            if best_code is not None and best_code != seg.tob_code:
                if verbose:
                    sy, sm = from_absolute_month(seg.start_month)
                    ey, em = from_absolute_month(seg.end_month)
                    log(f"  Recoding short segment {sy}-{sm:02d}–{ey}-{em:02d}: "
                        f"{seg.tob_code} → {best_code} "
                        f"(distinct {current_distinct} → {best_distinct})")
                result[i] = TobSegment(
                    start_month=seg.start_month,
                    end_month=seg.end_month,
                    tob_code=best_code,
                    variance_score=seg.variance_score,
                    include_in_his=seg.include_in_his,
                    start_day=seg.start_day,
                    end_day=seg.end_day
                )
                changed = True

    return result


# ==============================================================================
# Phase 7: Cross-Gap TOB Attribution
# ==============================================================================

def cross_gap_attribution(
    segments: List[TobSegment],
    residuals: Dict[Tuple[int, int], float],
    basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
    verbose: bool = False
) -> List[TobSegment]:
    """Recode short near-side bridge segments to the far-side TOB code when
    PHA continuity is confirmed across a data gap.

    When a short bridge segment (< MIN_SEGMENT_MONTHS data points) has the
    same TOB code as the preceding include_in_his segment (the "near-side"
    code), it may have been assigned that code simply because it is closer to
    the near side of a data gap.  If the next include_in_his segment carries a
    different code (the "far-side" code), we test whether extending the
    far-side code backwards to cover the bridge is consistent with a single,
    continuous PHA adjustment across the gap:

      * Collect up to CROSS_GAP_WINDOW months of residuals from the near-side
        segment, subtract the near-side basis → pre_cents (integer cents).
      * Collect up to CROSS_GAP_WINDOW months of residuals from the far-side
        segment, subtract the far-side basis → post_cents.
      * If len(pre_cents | post_cents) ≤ 3, the PHA is continuous across the
        gap — the bridge should carry the far-side code.
      * Additionally verify that the bridge itself is consistent with the
        far-side code (distinct ≤ 3).

    When the test passes the bridge is recoded to the far-side code and its
    include_in_his flag is set to True (it now marks a genuine TOB transition).
    Phase 8 (merge_segments) will then fold the bridge into the far-side
    segment if no MSHR record separates them.
    """
    if len(segments) <= 1:
        return segments

    CROSS_GAP_WINDOW = 36
    MAX_DISTINCT = 3

    result = list(segments)

    for i, seg in enumerate(result):
        # Only look at PHA-only (include_in_his=False) segments
        if seg.include_in_his:
            continue

        # Effective data window (in case adjacent segments overlap)
        eff_start = seg.start_month
        eff_end = seg.end_month
        if i > 0 and result[i - 1].end_month >= eff_start:
            eff_start = result[i - 1].end_month + 1
        if i < len(result) - 1 and result[i + 1].start_month <= eff_end:
            eff_end = result[i + 1].start_month - 1

        bridge_count = sum(
            1 for (y, m) in residuals
            if eff_start <= to_absolute_month(y, m) <= eff_end
        )
        if bridge_count >= MIN_SEGMENT_MONTHS:
            continue

        near_code = seg.tob_code

        # Find preceding include_in_his=True segment (near-side)
        prev_his_idx = None
        for j in range(i - 1, -1, -1):
            if result[j].include_in_his:
                prev_his_idx = j
                break
        if prev_his_idx is None:
            continue
        prev_his_seg = result[prev_his_idx]
        if prev_his_seg.tob_code != near_code:
            continue  # bridge code must match the near-side segment
        if near_code not in basis_vectors:
            continue

        # Find next include_in_his=True segment; it must carry a different code.
        # Any intervening include_in_his=False segment means this is not a simple
        # gap bridge (actual data exists between the bridge and the far side).
        far_seg_idx = None
        for j in range(i + 1, len(result)):
            if result[j].include_in_his:
                if result[j].tob_code != near_code:
                    far_seg_idx = j
                break  # stop at first include_in_his=True regardless
            else:
                break  # intervening PHA-only segment → not a gap bridge
        if far_seg_idx is None:
            continue

        far_seg = result[far_seg_idx]
        far_code = far_seg.tob_code
        if far_code not in basis_vectors:
            continue

        # Collect pre-bridge residual cents (using near-side basis)
        near_basis = basis_vectors[near_code]
        pre_win_end = prev_his_seg.end_month
        pre_win_start = max(prev_his_seg.start_month,
                            pre_win_end - CROSS_GAP_WINDOW + 1)
        pre_cents = set()
        for (y, m), r in residuals.items():
            abs_m = to_absolute_month(y, m)
            if pre_win_start <= abs_m <= pre_win_end and (y, m) in near_basis:
                pre_cents.add(int(round((r - near_basis[(y, m)]) * 100)))

        if not pre_cents:
            continue

        # Collect post-bridge residual cents (using far-side basis)
        far_basis = basis_vectors[far_code]
        post_win_start = far_seg.start_month
        post_win_end = min(far_seg.end_month,
                           post_win_start + CROSS_GAP_WINDOW - 1)
        post_cents = set()
        for (y, m), r in residuals.items():
            abs_m = to_absolute_month(y, m)
            if post_win_start <= abs_m <= post_win_end and (y, m) in far_basis:
                post_cents.add(int(round((r - far_basis[(y, m)]) * 100)))

        if not post_cents:
            continue

        # PHA-continuity check: combined distinct ≤ MAX_DISTINCT
        combined = pre_cents | post_cents
        if len(combined) > MAX_DISTINCT:
            continue

        # Bridge consistency check with far-side code
        bridge_distinct_far = count_segment_distinct_values(
            residuals, far_basis, eff_start, eff_end
        )
        if bridge_distinct_far > MAX_DISTINCT:
            continue

        # Also check bridge consistency with the near-side code.
        # Only recode to far-side if the bridge is strictly MORE consistent
        # with the far-side code (fewer distinct values).  If the bridge is
        # equally or more consistent with the near-side, keep it as-is.
        bridge_distinct_near = count_segment_distinct_values(
            residuals, near_basis, eff_start, eff_end
        )
        if bridge_distinct_near <= bridge_distinct_far:
            continue

        # All checks passed — recode bridge to far-side code
        if verbose:
            sy, sm = from_absolute_month(seg.start_month)
            ey, em = from_absolute_month(seg.end_month)
            log(f"  Cross-gap attribution {sy}-{sm:02d}–{ey}-{em:02d}: "
                f"{near_code} → {far_code} "
                f"(pre={len(pre_cents)} post={len(post_cents)} "
                f"combined={len(combined)} bridge_near={bridge_distinct_near} bridge_far={bridge_distinct_far})")

        result[i] = TobSegment(
            start_month=seg.start_month,
            end_month=seg.end_month,
            tob_code=far_code,
            variance_score=seg.variance_score,
            include_in_his=True,
            start_day=seg.start_day,
            end_day=seg.end_day
        )

    return result


# ==============================================================================
# Phase 8: Segment Merging
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


def trim_anomalous_leading_months(
        segments: List[TobSegment],
        residuals: Dict[Tuple[int, int], float],
        basis_vectors: Dict[str, Dict[Tuple[int, int], float]],
        verbose: bool = False) -> List[TobSegment]:
    """Phase 8.5: Trim anomalous leading months from the first segment.

    After merge_segments, the first include_in_his=True segment may have leading
    data months whose adjusted residuals (R - S_code) don't match the stable level
    of the rest of the segment.  This happens when:
      - NOAA's QCF assumed no TOB correction (24HR) for the very first few months,
      - those months were then merged with a later sub-segment of the same code.

    Detection: scan leading data months and trim those whose adjusted residual
    (R - S_code) is UNIQUE — i.e. the value does not appear anywhere else in the
    segment.  Such months have anomalous residuals that cannot be explained by a
    normal PHA step shared with later data; they likely reflect a mismatch between
    the assumed TOB code and what NOAA's QCF actually assumed for those months.

    Trimming stops as soon as a leading month's residual is non-unique (appears
    elsewhere in the segment) or MAX_TRIM months have been removed.

    Only applies to the first include_in_his=True segment when its code ≠ 24HR.
    """
    MAX_TRIM = 4

    # Find first include_in_his=True segment
    first_idx = next((i for i, s in enumerate(segments) if s.include_in_his), None)
    if first_idx is None:
        return segments

    seg = segments[first_idx]
    if seg.tob_code == TOB_NO_BIAS_CODE:
        return segments
    if seg.tob_code not in basis_vectors:
        return segments

    basis = basis_vectors[seg.tob_code]

    # Get sorted data months in this segment that have a basis value
    data_months = sorted(
        to_absolute_month(y, m) for (y, m) in residuals
        if seg.start_month <= to_absolute_month(y, m) <= seg.end_month
        and (y, m) in basis
    )
    if len(data_months) < MAX_TRIM + MIN_PERFECT_FIT_MONTHS:
        return segments

    # Compute adjusted residuals for all data months
    all_adj = [
        int(round((residuals[from_absolute_month(am)] - basis[from_absolute_month(am)]) * 100))
        for am in data_months
    ]

    # Find leading months whose adj value is unique (doesn't appear later)
    best_k = 0
    for k in range(min(MAX_TRIM, len(data_months) - MIN_PERFECT_FIT_MONTHS)):
        # Is this leading month's adj value unique (not in the rest of the segment)?
        if all_adj[k] not in set(all_adj[k + 1:]):
            best_k = k + 1  # Trim through this month
        else:
            break  # This month's value appears later → not anomalous, stop

    if best_k == 0:
        return segments  # No unique leading months to trim

    prefix_end = data_months[best_k - 1]   # Last month of 24HR prefix
    main_start = data_months[best_k]        # First month of trimmed segment

    if verbose:
        py, pm = from_absolute_month(prefix_end)
        my, mm = from_absolute_month(main_start)
        log(f"  Trimmed {best_k} leading months from first segment: "
            f"24HR prefix through {py}-{pm:02d}, "
            f"{seg.tob_code} starts {my}-{mm:02d} "
            f"(unique leading adj values: {[all_adj[i] for i in range(best_k)]})")

    prefix_seg = TobSegment(
        start_month=seg.start_month,
        end_month=prefix_end,
        tob_code=TOB_NO_BIAS_CODE,
        variance_score=0.0,
        data_count=best_k,
        include_in_his=True,
    )
    trimmed_seg = TobSegment(
        start_month=main_start,
        end_month=seg.end_month,
        tob_code=seg.tob_code,
        variance_score=seg.variance_score,
        data_count=seg.data_count - best_k,
        include_in_his=seg.include_in_his,
        start_day=seg.start_day,
        end_day=seg.end_day,
    )

    result = list(segments)
    result[first_idx] = trimmed_seg
    result.insert(first_idx, prefix_seg)
    return result


# ==============================================================================
# Validation (copied from v2)
# ==============================================================================

def validate_segments(station_id: str, segments: List[TobSegment],
                     qcu_data: Dict[Tuple[int, int], int],
                     qcf_data: Dict[Tuple[int, int], int],
                     inv_entry: InventoryEntry,
                     tob_bin: Path,
                     qcu_file: Path,
                     mshr_records: Optional[List[MshrRecord]] = None,
                     use_mshr_lat_lon: bool = False,
                     verbose: bool = False,
                     debug_dir: Optional[Path] = None) -> bool:
    """Validate reconstructed TOB history and check for remaining exceedances."""

    if not segments:
        return True

    if mshr_records is None:
        mshr_records = []
    inv_elev_ft = int(round(inv_entry.elev * 3.28084))

    with tempfile.TemporaryDirectory() as temp_dir:
        work_dir = Path(temp_dir)
        his_file = work_dir / f"{station_id}.his"

        with open(his_file, 'w') as f:
            for seg in segments:
                start_y, start_m = from_absolute_month(seg.start_month)
                end_y, end_m = from_absolute_month(seg.end_month)

                begin_date = dt.date(start_y, start_m, 1)
                end_date = dt.date(end_y, end_m, 28)

                if use_mshr_lat_lon:
                    active = _active_mshr_at(begin_date, mshr_records)
                    if active and active.lat is not None and active.lon is not None:
                        lat_deg, lat_min, lat_sec = decimal_to_dms(active.lat)
                        lon_deg, lon_min, lon_sec = decimal_to_dms(active.lon)
                    else:
                        lat_deg, lat_min, lat_sec = decimal_to_dms(inv_entry.lat)
                        lon_deg, lon_min, lon_sec = decimal_to_dms(inv_entry.lon)
                    elev = active.elev_ft if (active and active.elev_ft is not None) else inv_elev_ft
                else:
                    lat_deg, lat_min, lat_sec = decimal_to_dms(inv_entry.lat)
                    lon_deg, lon_min, lon_sec = decimal_to_dms(inv_entry.lon)
                    elev = int(round(inv_entry.elev))

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

        tob_data = run_tobmain(tob_bin, station_id, qcu_file, his_file, work_dir, inv_entry,
                               use_mshr_lat_lon, verbose, debug_dir)

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

    Includes segments with include_in_his=True (TOB-related boundaries), with
    MSHR record begin dates inserted as additional split points so that every
    MSHR entry produces at least one output row.  Rows are always contiguous.
    """

    his_path = out_dir / f"{station_id}.his"

    # Filter segments to include only TOB-related boundaries
    his_segments = [seg for seg in segments if seg.include_in_his]

    # --- Step 1: compute the actual begin/end date for each his_segment -------
    # Non-last segments extend to the day before the next segment starts so that
    # rows are contiguous even when include_in_his=False segments sit between them.
    seg_ranges = []  # (begin_date, end_date, tob_code)
    for i, seg in enumerate(his_segments):
        start_y, start_m = from_absolute_month(seg.start_month)
        start_day = seg.start_day if seg.start_day is not None else 1
        begin_date = dt.date(start_y, start_m, start_day)

        if i < len(his_segments) - 1:
            next_seg = his_segments[i + 1]
            next_start_y, next_start_m = from_absolute_month(next_seg.start_month)
            next_start_day = next_seg.start_day if next_seg.start_day is not None else 1
            end_date = dt.date(next_start_y, next_start_m, next_start_day) - dt.timedelta(days=1)
        else:
            end_y, end_m = from_absolute_month(seg.end_month)
            if seg.end_day is not None:
                end_day = seg.end_day
            else:
                _, end_day = calendar.monthrange(end_y, end_m)
            end_date = dt.date(end_y, end_m, end_day)

        seg_ranges.append((begin_date, end_date, seg.tob_code))

    # --- Step 2: build output rows, inserting MSHR boundaries ----------------
    # Each row is (begin_date, tob_code).
    #
    # For each his_segment we add the segment's own begin_date, then any MSHR
    # begin_date that falls strictly within that segment's range.
    #
    # MSHR records that begin before the first his_segment are NOT added as
    # explicit split points, but they ARE implicitly covered: the very first row
    # starts at first_seg_begin, which effectively trims the pre-data portion of
    # any MSHR that was active at that point.  The next MSHR's begin_date (which
    # falls within the segment range) is added explicitly, so the transition is
    # still captured.  MSHR records whose end_date is also before first_seg_begin
    # are irrelevant and correctly receive no row at all.
    #
    # MSHR records that begin after the last his_segment end are added as
    # trailing rows using the final TOB code and using the MSHR's own end_date
    # for the very last row.
    last_seg_end = seg_ranges[-1][1] if seg_ranges else None
    last_tob_code = seg_ranges[-1][2] if seg_ranges else TOB_NO_BIAS_CODE

    rows: List[Tuple[dt.date, str]] = []
    for seg_begin, seg_end, tob_code in seg_ranges:
        rows.append((seg_begin, tob_code))
        for mshr in mshr_records:
            if seg_begin < mshr.begin_date <= seg_end:
                rows.append((mshr.begin_date, tob_code))

    # Trailing MSHR records: begin after the last his_segment ends.
    trailing_mshr: List[MshrRecord] = []
    if last_seg_end is not None:
        trailing_mshr = sorted(
            [mshr for mshr in mshr_records if mshr.begin_date > last_seg_end],
            key=lambda m: m.begin_date
        )
    for mshr in trailing_mshr:
        rows.append((mshr.begin_date, last_tob_code))

    # Sort and deduplicate (a TOB boundary and an MSHR boundary may coincide)
    rows.sort(key=lambda x: x[0])
    seen: Set[dt.date] = set()
    deduped: List[Tuple[dt.date, str]] = []
    for begin_date, tob_code in rows:
        if begin_date not in seen:
            seen.add(begin_date)
            deduped.append((begin_date, tob_code))
    rows = deduped

    # The overall end date: trailing MSHR records extend beyond the data range,
    # so use the last trailing MSHR's own end_date; otherwise the last his_segment's end.
    last_end_date = trailing_mshr[-1].end_date if trailing_mshr else last_seg_end

    # --- Step 3: write rows ---------------------------------------------------
    # Build a lookup from begin_date → MshrRecord for O(1) boundary checks.
    mshr_by_date: Dict[dt.date, MshrRecord] = {m.begin_date: m for m in mshr_records}

    # Inventory elevation converted to feet as a fallback (MSHR uses feet).
    inv_elev_ft = int(round(inv_entry.elev * 3.28084))

    def _active_mshr(date: dt.date) -> Optional[MshrRecord]:
        """Return the MSHR record with the latest begin_date <= date, or None."""
        result = None
        for m in mshr_records:  # sorted by begin_date
            if m.begin_date <= date:
                result = m
            else:
                break
        return result

    with open(his_path, 'w') as f:
        for i, (begin_date, tob_code) in enumerate(rows):
            if i < len(rows) - 1:
                end_date = rows[i + 1][0] - dt.timedelta(days=1)
            else:
                end_date = last_end_date

            # Lat/lon/elev from the MSHR record active at this date; fall back
            # to the inventory entry if MSHR data is absent.
            active = _active_mshr(begin_date)
            if active and active.lat is not None and active.lon is not None:
                lat_deg, lat_min, lat_sec = decimal_to_dms(active.lat)
                lon_deg, lon_min, lon_sec = decimal_to_dms(active.lon)
            else:
                lat_deg, lat_min, lat_sec = decimal_to_dms(inv_entry.lat)
                lon_deg, lon_min, lon_sec = decimal_to_dms(inv_entry.lon)
            elev = active.elev_ft if (active and active.elev_ft is not None) else inv_elev_ft

            # Relocation: non-blank only when an MSHR boundary starts exactly
            # here and that MSHR record documents a relocation.
            boundary = mshr_by_date.get(begin_date)
            if boundary and boundary.relocation.strip():
                dist_dir = boundary.relocation[:11].ljust(11)
            else:
                dist_dir = ' ' * 11

            line = (
                f"2"
                f"{' ' * 12}"
                f" {begin_date.year:4d}{begin_date.month:02d}{begin_date.day:02d}"
                f" {end_date.year:4d}{end_date.month:02d}{end_date.day:02d}"
                f" {lat_deg:3.0f}{lat_min:3.0f}{lat_sec:3.0f}"
                f" {lon_deg:4.0f}{lon_min:3.0f}{lon_sec:3.0f}"
                f" {dist_dir}"
                f" {elev:5d}"
                f"  {' ' * 4}"
                f" {tob_code:4s}"
                f"     {' ' * 66}"
            )
            f.write(line + "\n")

    if verbose:
        log(f"  Wrote {his_path} ({len(rows)} rows, {len(his_segments)} TOB segments, "
            f"{len(mshr_records)} MSHR records)")


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
    parser.add_argument(
        '--mshr-tob-lat-lon', dest='mshr_tob_lat_lon', action='store_true', default=False,
        help='Use per-period MSHR lat/lon when fitting TOB basis vectors instead of the '
             'station inventory lat/lon. Does not affect which lat/lon appears in the '
             'output .his — to omit MSHR from the output, omit --mshr-zip'
    )
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

        station_mshr = mshr_records.get(station_id, [])

        # Phase 1: Generate basis vectors
        basis_vectors = generate_basis_vectors(
            station_id, inv_entry, qcu_data, residuals,
            tob_bin, qcu_file,
            station_mshr, args.mshr_tob_lat_lon,
            args.verbose, debug_dir
        )

        if not basis_vectors:
            log(f"  ERROR: Could not generate basis vectors for {station_id}")
            continue

        # Phase 2: Forward scanning
        segments = forward_scan(residuals, basis_vectors, args.verbose)

        if not segments:
            log(f"  ERROR: Forward scan produced no segments for {station_id}")
            continue

        # Phase 3: Validate and adjust boundary timing
        segments = validate_boundary_timing(segments, residuals, basis_vectors, args.verbose)

        # Phase 3b: Back-attribute trailing months that match next segment's level
        segments = back_attribute_trailing_months(segments, residuals, basis_vectors, args.verbose)

        # Phase 4: Refine spike boundaries with optimal day-of-month
        segments = refine_spike_boundaries(segments, residuals, basis_vectors, args.verbose)

        # Phase 4b: Heal multi-month edge anomalies at code-changing boundaries
        segments = heal_edge_transitions(segments, residuals, basis_vectors, args.verbose)

        # Phase 5: Fix pathological segmentation (segments < 12 months)
        segments = detect_and_fix_pathological_segmentation(
            station_id, segments, residuals, basis_vectors, args.verbose
        )

        # Phase 6: Recode short bridge segments to enable merging
        segments = recode_short_segments(segments, residuals, basis_vectors, args.verbose)

        # Phase 7: Cross-gap TOB attribution
        segments = cross_gap_attribution(segments, residuals, basis_vectors, args.verbose)

        if len(segments) > 1:
            stats['transitions_detected'] += 1

        # Phase 8: Merge same-code segments
        segments = merge_segments(segments, station_mshr, args.verbose)

        # Phase 8.5: Trim anomalous leading months from first segment (add 24HR prefix)
        segments = trim_anomalous_leading_months(segments, residuals, basis_vectors, args.verbose)

        # Validation
        is_valid = validate_segments(
            station_id, segments, qcu_data, qcf_data, inv_entry,
            tob_bin, qcu_file,
            station_mshr, args.mshr_tob_lat_lon,
            args.verbose, debug_dir
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
