#!/usr/bin/env python3
"""Shared I/O, data model, and utilities for GHCN-M TOB history reconstruction.

Neutral building blocks used by the TOB history solver (qcuf_pattern_to_his.py):
GHCN-M inventory / station-data / MSHR parsing, the TOB code tables, the
InventoryEntry / MshrRecord / TobSegment data classes, and month/coordinate
helpers.  Contains no reconstruction algorithm -- just the pieces every approach
needs.
"""

import datetime as dt
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

QUICK_FILTER_THRESHOLD = 6      # Distinct values in a 24-month window

# TOB codes.  00HR and 24HR are both midnight; we only include 24HR.
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


# ==============================================================================
# Utility functions
# ==============================================================================

def log(msg: str, flush: bool = True):
    """Print message with optional flush."""
    print(msg, flush=flush)


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
# File parsing
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
                #   LAT_DEC      1300-1319  -> [1299:1319]
                #   LON_DEC      1321-1340  -> [1320:1340]
                #   ELEV_GROUND   990-1029  -> [ 989:1029]  (in feet)
                #   RELOCATION   1353-1414  -> [1352:1414]
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
