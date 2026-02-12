#!/usr/bin/env python3
"""Generate station .his files from HOMR PHR fixed-width data.

Fixed-width parsing only. No whitespace parsing.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import sys
import zipfile
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple


@dataclass(frozen=True)
class InventoryEntry:
    station_id: str
    lat: float
    lon: float
    elev: float


@dataclass(frozen=True)
class PhrRecord:
    station_id: str
    begin_date: dt.date
    end_date: dt.date
    obs_time: str  # HHHR


@dataclass(frozen=True)
class MshrRecord:
    station_id: str
    begin_date: dt.date
    end_date: dt.date
    lat: Optional[float]
    lon: Optional[float]
    elev: Optional[float]
    relocation: str


@dataclass(frozen=True)
class LocationInfo:
    lat: Optional[float]
    lon: Optional[float]
    elev: Optional[float]
    relocation: str


def choose_location_info(
    phr: Optional[PhrRecord],
    mshr: Optional[MshrRecord],
    inv: Optional[InventoryEntry],
    last: LocationInfo,
    prefer_mshr: bool,
) -> LocationInfo:
    # PHR currently provides no location fields, but keep this priority for future extensions:
    # PHR > MSHR > Inventory > last-known.
    if phr is not None:
        # No location in PHR yet, fall through.
        pass
    if mshr is not None and mshr.lat is not None and mshr.lon is not None:
        return LocationInfo(mshr.lat, mshr.lon, mshr.elev, mshr.relocation)
    if not prefer_mshr and inv is not None:
        return LocationInfo(inv.lat, inv.lon, inv.elev, "")
    if last.lat is not None and last.lon is not None:
        return last
    return LocationInfo(0.0, 0.0, 0.0, "")


PHR_SLICES = {
    "ghcnd_id": (85, 105),
    "begin_date": (106, 114),
    "end_date": (115, 123),
    "element": (124, 134),
    "data_program": (135, 165),
    "frequency": (166, 196),
    "time_of_obs": (197, 205),
}


SPECIAL_TOBS = {"9999", "8888", "7777", "7070", "6666", "5555", "4444", "3030"}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build .his files from PHR fixed-width data")
    parser.add_argument("--phr-zip", required=True, help="Path to phr.txt.zip")
    parser.add_argument("--phr-member", help="Member filename inside zip (defaults to first)")
    parser.add_argument("--mshr-zip", help="Path to mshr_enhanced.txt.zip")
    parser.add_argument("--mshr-member", help="Member filename inside mshr zip (defaults to first)")
    parser.add_argument("--inventory", help="Path to fixed-width GHCNM inventory file")
    parser.add_argument("--out-dir", required=True, help="Output directory for .his files")
    parser.add_argument("--include-prefixes", default="USC,USW", help="Comma-separated station ID prefixes")
    parser.add_argument("--contig-bbox", default="22,51,-127,-64",
                        help="Lat/Lon bbox as min_lat,max_lat,min_lon,max_lon")
    parser.add_argument("--begin-year", type=int, help="Optional clamp begin year")
    parser.add_argument("--end-year", type=int, help="Optional clamp end year")
    parser.add_argument("--dry-run", action="store_true", help="Do not write files; print summary only")
    return parser.parse_args()


def parse_inventory(path: Path) -> Dict[str, InventoryEntry]:
    entries: Dict[str, InventoryEntry] = {}
    bad_lines = 0
    with path.open("r", encoding="utf-8", errors="replace") as fh:
        for line in fh:
            if len(line) < 37:
                bad_lines += 1
                continue
            station_id = line[0:11]
            try:
                lat = float(line[12:20])
                lon = float(line[21:30])
                elev = float(line[31:37])
            except ValueError:
                bad_lines += 1
                continue
            entries[station_id] = InventoryEntry(station_id=station_id, lat=lat, lon=lon, elev=elev)
    if bad_lines:
        print(f"Inventory parse skipped {bad_lines} lines", file=sys.stderr)
    return entries


def parse_date(yyyymmdd: str) -> Optional[dt.date]:
    if len(yyyymmdd) != 8 or not yyyymmdd.isdigit():
        return None
    year = int(yyyymmdd[0:4])
    month = int(yyyymmdd[4:6])
    day = int(yyyymmdd[6:8])
    try:
        return dt.date(year, month, day)
    except ValueError:
        return None


def map_time_of_obs(raw: str) -> Optional[str]:
    raw = raw.strip()
    if not raw or raw in SPECIAL_TOBS:
        return None
    if len(raw) != 4 or not raw.isdigit():
        return None
    if raw == "0000":
        return "24HR"
    if raw == "0630":
        return "06HR"
    if raw == "1830":
        return "18HR"
    hour = raw[0:2]
    if not hour.isdigit():
        return None
    return f"{hour}HR"


def in_bbox(entry: InventoryEntry, bbox: Tuple[float, float, float, float]) -> bool:
    min_lat, max_lat, min_lon, max_lon = bbox
    return min_lat <= entry.lat <= max_lat and min_lon <= entry.lon <= max_lon


def parse_float_field(raw: str) -> Optional[float]:
    raw = raw.strip()
    if not raw:
        return None
    try:
        return float(raw)
    except ValueError:
        return None


def read_mshr_records(
    zip_path: Path,
    member: Optional[str],
    prefixes: Tuple[str, ...],
) -> Dict[str, List[MshrRecord]]:
    records: Dict[str, List[MshrRecord]] = {}
    with zipfile.ZipFile(zip_path) as zf:
        member_name = member or zf.namelist()[0]
        with zf.open(member_name) as fh:
            for raw in fh:
                try:
                    line = raw.decode("utf-8")
                except UnicodeDecodeError:
                    line = raw.decode("latin-1", errors="replace")

                station_id = line[239:259].strip()
                if not station_id or not station_id.startswith(prefixes):
                    continue

                begin_raw = line[32:40].strip()
                end_raw = line[41:49].strip()
                begin_date = parse_date(begin_raw)
                end_date = parse_date(end_raw)
                if begin_date is None or end_date is None:
                    continue

                lat = parse_float_field(line[1299:1319])
                lon = parse_float_field(line[1320:1340])
                elev = parse_float_field(line[989:1029])
                relocation = line[1352:1414].rstrip()

                records.setdefault(station_id, []).append(
                    MshrRecord(
                        station_id=station_id,
                        begin_date=begin_date,
                        end_date=end_date,
                        lat=lat,
                        lon=lon,
                        elev=elev,
                        relocation=relocation,
                    )
                )
    return records


def read_phr_records(
    zip_path: Path,
    member: Optional[str],
    inventory: Dict[str, InventoryEntry],
    prefixes: Tuple[str, ...],
    bbox: Tuple[float, float, float, float],
    begin_year: Optional[int],
    end_year: Optional[int],
) -> Tuple[List[PhrRecord], Dict[str, int]]:
    counts = {
        "lines": 0,
        "kept": 0,
        "missing_inventory": 0,
        "missing_time": 0,
        "bad_dates": 0,
        "filtered": 0,
    }
    records: List[PhrRecord] = []

    with zipfile.ZipFile(zip_path) as zf:
        member_name = member or zf.namelist()[0]
        with zf.open(member_name) as fh:
            for raw in fh:
                counts["lines"] += 1
                try:
                    line = raw.decode("utf-8")
                except UnicodeDecodeError:
                    line = raw.decode("latin-1", errors="replace")

                station_id = line[PHR_SLICES["ghcnd_id"][0]:PHR_SLICES["ghcnd_id"][1]].strip()
                if not station_id or not station_id.startswith(prefixes):
                    counts["filtered"] += 1
                    continue

                inv = inventory.get(station_id)
                if inventory and inv is None:
                    counts["missing_inventory"] += 1
                if inv is not None and not in_bbox(inv, bbox):
                    counts["filtered"] += 1
                    continue

                element = line[PHR_SLICES["element"][0]:PHR_SLICES["element"][1]].strip()
                if element != "TEMP":
                    counts["filtered"] += 1
                    continue

                data_program = line[PHR_SLICES["data_program"][0]:PHR_SLICES["data_program"][1]].strip()
                if "COOP SOD" not in data_program:
                    counts["filtered"] += 1
                    continue

                begin_raw = line[PHR_SLICES["begin_date"][0]:PHR_SLICES["begin_date"][1]].strip()
                end_raw = line[PHR_SLICES["end_date"][0]:PHR_SLICES["end_date"][1]].strip()
                begin_date = parse_date(begin_raw)
                end_date = parse_date(end_raw)
                if begin_date is None or end_date is None:
                    counts["bad_dates"] += 1
                    continue

                if begin_year is not None and begin_date.year < begin_year:
                    begin_date = dt.date(begin_year, 1, 1)
                if end_year is not None and end_date.year > end_year:
                    end_date = dt.date(end_year, 12, 31)
                if begin_date > end_date:
                    counts["filtered"] += 1
                    continue

                tob_raw = line[PHR_SLICES["time_of_obs"][0]:PHR_SLICES["time_of_obs"][1]].strip()
                obs_time = map_time_of_obs(tob_raw)
                if obs_time is None:
                    counts["missing_time"] += 1
                    continue

                records.append(PhrRecord(station_id=station_id, begin_date=begin_date, end_date=end_date, obs_time=obs_time))
                counts["kept"] += 1

    return records, counts


def merge_periods(periods: List[PhrRecord]) -> List[PhrRecord]:
    if not periods:
        return []
    periods = sorted(periods, key=lambda r: (r.begin_date, r.end_date))
    merged: List[PhrRecord] = [periods[0]]
    for rec in periods[1:]:
        last = merged[-1]
        if rec.obs_time == last.obs_time:
            if last.end_date >= dt.date.max - dt.timedelta(days=1):
                last_end_plus = dt.date.max
            else:
                last_end_plus = last.end_date + dt.timedelta(days=1)
            if rec.begin_date <= last_end_plus:
                new_end = max(last.end_date, rec.end_date)
                merged[-1] = PhrRecord(
                    station_id=last.station_id,
                    begin_date=last.begin_date,
                    end_date=new_end,
                    obs_time=last.obs_time,
                )
                continue
        merged.append(rec)
    return merged


def decimal_to_dms(value: float) -> Tuple[int, int, int]:
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


def format_his_line(
    source: int,
    begin_date: dt.date,
    end_date: dt.date,
    lat: float,
    lon: float,
    elev: int,
    obs_time: str,
    relocation: str,
) -> str:
    lat_deg, lat_min, lat_sec = decimal_to_dms(lat)
    lon_deg, lon_min, lon_sec = decimal_to_dms(lon)

    relocation_clean = relocation.strip()
    if relocation_clean:
        distance_and_direction = relocation_clean[:11].ljust(11)
    else:
        distance_and_direction = " " * 11
    elevation = elev
    instr_height_full = "    "
    station_instr = ("".join([" " * 5 + " " for _ in range(11)])).rstrip("\n")

    line = (
        f"{source:1d}"  # i1
        f"{' ' * 12}"
        f" {begin_date.year:4d}{begin_date.month:02d}{begin_date.day:02d}"
        f" {end_date.year:4d}{end_date.month:02d}{end_date.day:02d}"
        f" {lat_deg:3.0f}{lat_min:3.0f}{lat_sec:3.0f}"
        f" {lon_deg:4.0f}{lon_min:3.0f}{lon_sec:3.0f}"
        f" {distance_and_direction}"
        f" {elevation:5d}"
        f"  {instr_height_full}"
        f" {obs_time:4s}"
        f"     {station_instr}"
    )
    return line


def write_his_files(
    out_dir: Path,
    records: Iterable[PhrRecord],
    inventory: Dict[str, InventoryEntry],
    mshr_records: Dict[str, List[MshrRecord]],
    dry_run: bool,
) -> int:
    out_dir.mkdir(parents=True, exist_ok=True)
    by_station: Dict[str, List[PhrRecord]] = {}
    for rec in records:
        by_station.setdefault(rec.station_id, []).append(rec)

    stations_written = 0
    all_station_ids = set(by_station.keys()) | set(mshr_records.keys())

    for station_id in sorted(all_station_ids):
        phr_recs = merge_periods(by_station.get(station_id, []))
        mshr_recs = sorted(mshr_records.get(station_id, []), key=lambda r: (r.begin_date, r.end_date))

        inv = inventory.get(station_id)

        # Build boundary dates from PHR and MSHR records
        boundaries: List[dt.date] = []
        for rec in phr_recs:
            boundaries.append(rec.begin_date)
            if rec.end_date < dt.date.max:
                boundaries.append(rec.end_date + dt.timedelta(days=1))
        for rec in mshr_recs:
            boundaries.append(rec.begin_date)
            if rec.end_date < dt.date.max:
                boundaries.append(rec.end_date + dt.timedelta(days=1))
        if not boundaries:
            continue
        boundaries = sorted(set(boundaries))

        # Iterators for active records
        phr_idx = 0
        mshr_idx = 0

        def active_phr(start: dt.date) -> Optional[PhrRecord]:
            nonlocal phr_idx
            while phr_idx < len(phr_recs) and phr_recs[phr_idx].end_date < start:
                phr_idx += 1
            if phr_idx < len(phr_recs):
                rec = phr_recs[phr_idx]
                if rec.begin_date <= start <= rec.end_date:
                    return rec
            return None

        def active_mshr(start: dt.date) -> Optional[MshrRecord]:
            nonlocal mshr_idx
            while mshr_idx < len(mshr_recs) and mshr_recs[mshr_idx].end_date < start:
                mshr_idx += 1
            if mshr_idx < len(mshr_recs):
                rec = mshr_recs[mshr_idx]
                if rec.begin_date <= start <= rec.end_date:
                    return rec
            return None

        if dry_run:
            stations_written += 1
            continue

        out_path = out_dir / f"{station_id}.his"
        use_mshr_only = len(mshr_recs) > 0
        last_location = LocationInfo(None, None, None, "")
        if use_mshr_only:
            for rec in mshr_recs:
                if rec.lat is not None and rec.lon is not None:
                    last_location = LocationInfo(rec.lat, rec.lon, rec.elev, rec.relocation)
                    break

        with out_path.open("w", encoding="utf-8") as fh:
            for i in range(len(boundaries) - 1):
                start = boundaries[i]
                end = boundaries[i + 1] - dt.timedelta(days=1)

                phr_active = active_phr(start)
                mshr_active = active_mshr(start)

                if phr_active is None and mshr_active is None:
                    continue

                obs_time = phr_active.obs_time if phr_active is not None else "    "

                location = choose_location_info(phr_active, mshr_active, inv, last_location, prefer_mshr=use_mshr_only)
                lat = location.lat if location.lat is not None else 0.0
                lon = location.lon if location.lon is not None else 0.0
                elev_val = location.elev if location.elev is not None else 0.0
                relocation = location.relocation

                elev = int(round(elev_val)) if elev_val is not None else (int(round(inv.elev)) if inv else 0)
                last_location = LocationInfo(lat, lon, elev_val, relocation)
                line = format_his_line(2, start, end, lat, lon, elev, obs_time, relocation)
                fh.write(line.rstrip("\n") + "\n")
        stations_written += 1
    return stations_written


def parse_bbox(text: str) -> Tuple[float, float, float, float]:
    parts = list(csv.reader([text]))[0]
    if len(parts) != 4:
        raise ValueError("contig-bbox must have 4 comma-separated values")
    return tuple(float(p) for p in parts)  # type: ignore[return-value]


def main() -> int:
    args = parse_args()
    inv: Dict[str, InventoryEntry] = {}
    if args.inventory:
        inv = parse_inventory(Path(args.inventory))
    prefixes = tuple(p.strip().upper() for p in args.include_prefixes.split(",") if p.strip())
    bbox = parse_bbox(args.contig_bbox)

    mshr_records: Dict[str, List[MshrRecord]] = {}
    if args.mshr_zip:
        mshr_records = read_mshr_records(Path(args.mshr_zip), args.mshr_member, prefixes)

    records, counts = read_phr_records(
        zip_path=Path(args.phr_zip),
        member=args.phr_member,
        inventory=inv,
        prefixes=prefixes,
        bbox=bbox,
        begin_year=args.begin_year,
        end_year=args.end_year,
    )

    # Filter by bbox using inventory if provided
    if bbox and inv:
        filtered_records: List[PhrRecord] = []
        for rec in records:
            inv_entry = inv.get(rec.station_id)
            if inv_entry and in_bbox(inv_entry, bbox):
                filtered_records.append(rec)
        records = filtered_records

    stations_written = write_his_files(Path(args.out_dir), records, inv, mshr_records, args.dry_run)

    print("Summary")
    print(f"  PHR lines read:        {counts['lines']}")
    print(f"  Records kept:          {counts['kept']}")
    print(f"  Stations written:      {stations_written}")
    print(f"  Missing inventory:     {counts['missing_inventory']}")
    print(f"  Missing/invalid TOBS:  {counts['missing_time']}")
    print(f"  Bad dates:             {counts['bad_dates']}")
    print(f"  Filtered (other):      {counts['filtered']}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
