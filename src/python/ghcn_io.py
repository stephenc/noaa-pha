#!/usr/bin/env python3
"""Fixed-width I/O for the GHCN-M v4 PHA/TOB pipeline.

Pure stdlib.  All parsing is fixed-width per the Fortran readers:

  * station.inv           (a11,f9.4,f10.4) + elev/name columns
  * per-station data      (a11,a1,i4,12(i6,a3))     [124-char lines]
  * .his history rows     FORMAT 90, see below
  * MSHR enhanced / PHR   HOMR fixed-width reports (zip archives)

.his FORMAT 90 (TOBMain.f95:470-471 == ReadInputFiles.f95:572):

  (i1,12x,2(1x,i4,2i2),1x,f3.0,2f3.0,1x,f4.0,2f3.0,1x,a11,1x,i5,2x,a4,1x,a4,
   5x,11(a5,1x))

Field-by-field 1-based columns (width accumulates left to right):
  i1        col 1        source
  12x       cols 2-13    (station id by convention; unread by Fortran)
  1x,i4,2i2 cols 14-22   beg: 14 blank, year 15-18, month 19-20, day 21-22
  1x,i4,2i2 cols 23-31   end: 23 blank, year 24-27, month 28-29, day 30-31
  1x        col 32
  f3.0 x3   cols 33-41   lat deg 33-35, min 36-38, sec 39-41
  1x        col 42
  f4.0      cols 43-46   lon deg (signed)
  f3.0 x2   cols 47-52   lon min 47-49, sec 50-52
  1x        col 53
  a11       cols 54-64   distance/direction
  1x        col 65
  i5        cols 66-70   elevation (feet)
  2x        cols 71-72
  a4        cols 73-76   instrument height
  1x        col 77
  a4        cols 78-81   observation time
  5x        cols 82-86
  11(a5,1x) cols 87-152  instruments (a5 at 87-91, 93-97, ... 147-151)

Total width: 152 characters.
"""

from __future__ import annotations

import zipfile
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

MISSING = -9999
HIS_ROW_WIDTH = 152
DATA_ROW_WIDTH = 124

# 24 hourly codes (24 == midnight == no adjustment) plus the seasonal
# specials.  TRID is excluded: it resolves like 00SS and would duplicate
# that basis column.
BASIS_CODES = [f"{h:02d}HR" for h in range(1, 25)] + ["00RS", "00SR", "00SS"]

PHR_OBS_SENTINELS = {"9999", "8888", "7777", "7070", "6666", "5555", "4444", "3030"}


# ---------------------------------------------------------------------------
# Fortran-style field helpers
# ---------------------------------------------------------------------------


def _fint(s: str) -> int:
    """Fortran integer READ: blank field reads as 0."""
    s = s.strip()
    return int(s) if s else 0


def _ffloat(s: str) -> float:
    """Fortran f-edit READ with .0 scale: blank reads as 0.0."""
    s = s.strip()
    if not s:
        return 0.0
    return float(s)


def decode_obtime(obtime: str) -> int:
    """Port of TOBUtils.f95 decode_obtime (lines 554-606).

    NOTE: PHA's ReadInputFiles.f95:637-643 extracts the 2-char sub-code with a
    DIFFERENT rule (char1=='9' and char2/='9' -> chars 2-3).  This function is
    the TOB-side decode, which is the one that determines adjustments.
    """
    obtime = f"{obtime:<4s}"[:4]
    if obtime == "TRID":
        return 30
    if obtime[2:4] == "HR":
        hh = obtime[0:2].strip()
        if not hh:
            code = 0
        else:
            try:
                code = int(hh)
            except ValueError:
                return 99
        if code == 0:
            code = 24  # hour 00 == midnight
        return code
    if obtime[0] == "9" and obtime[3] == "9" and obtime not in ("9909", "9919"):
        sub = obtime[1:3]
    else:
        sub = obtime[2:4]
    if sub in ("00", "0 "):
        return 24
    if sub in ("RS", "VR", "VA"):
        return 26
    if sub == "SR":
        return 27
    if sub in ("SS", "PM"):
        return 28
    if sub in ("99", "  ", "DE", "MI", "31", "73", "78", "83", "89", "UN"):
        return 99
    try:
        code = int(sub)
    except ValueError:
        return 99
    if code > 30 and code != 99:
        return 99
    return code


# ---------------------------------------------------------------------------
# Inventory
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class Inv:
    station_id: str
    lat: float
    lon: float
    elev_m: Optional[float]
    name: str


def read_inventory(path: Path) -> Dict[str, Inv]:
    out: Dict[str, Inv] = {}
    with open(path, "r", encoding="latin-1") as fh:
        for line in fh:
            line = line.rstrip("\n")
            if len(line) < 37:
                continue
            sid = line[0:11].strip()
            if not sid:
                continue
            try:
                lat = float(line[12:20])
                lon = float(line[21:30])
            except ValueError:
                continue
            try:
                elev: Optional[float] = float(line[31:37])
            except ValueError:
                elev = None
            if elev is not None and elev <= -999.0:
                elev = None
            name = line[38:].strip() if len(line) > 38 else ""
            out[sid] = Inv(sid, lat, lon, elev, name)
    return out


# ---------------------------------------------------------------------------
# Per-station data files  (a11,a1,i4,12(i6,a3))
# ---------------------------------------------------------------------------


@dataclass
class StationData:
    station_id: str
    data_type: str = " "
    # values: only non-missing months; flags: every month of every listed year
    values: Dict[Tuple[int, int], int] = field(default_factory=dict)
    flags: Dict[Tuple[int, int], str] = field(default_factory=dict)
    year_list: List[int] = field(default_factory=list)

    @property
    def years(self) -> List[int]:
        return sorted(self.year_list)


def read_station_data(path: Path) -> StationData:
    sd = StationData(station_id="")
    with open(path, "r", encoding="latin-1") as fh:
        for line in fh:
            line = line.rstrip("\n")
            if not line.strip():
                continue
            line = f"{line:<{DATA_ROW_WIDTH}s}"
            sid = line[0:11]
            if not sd.station_id:
                sd.station_id = sid.strip()
                sd.data_type = line[11:12]
            year = _fint(line[12:16])
            sd.year_list.append(year)
            for m in range(12):
                v = _fint(line[16 + 9 * m : 22 + 9 * m])
                fl = line[22 + 9 * m : 25 + 9 * m]
                sd.flags[(year, m + 1)] = fl
                if v != MISSING:
                    sd.values[(year, m + 1)] = v
    return sd


def write_station_data(path: Path, sd: StationData, data_type_char: str = " ") -> None:
    with open(path, "w", encoding="latin-1") as fh:
        for year in sorted(sd.year_list):
            parts = [f"{sd.station_id:<11s}", data_type_char[:1] or " ", f"{year:4d}"]
            for m in range(1, 13):
                v = sd.values.get((year, m), MISSING)
                fl = sd.flags.get((year, m), "   ")
                parts.append(f"{v:6d}{fl:<3s}")
            fh.write("".join(parts) + "\n")


# ---------------------------------------------------------------------------
# MSHR enhanced / PHR (HOMR fixed-width reports inside zips)
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class MshrRec:
    station_id: str
    begin: Optional[Tuple[int, int, int]]
    end: Optional[Tuple[int, int, int]]
    lat: Optional[float]
    lon: Optional[float]
    elev_ft: Optional[float]
    relocation: str


@dataclass(frozen=True)
class PhrRec:
    station_id: str
    begin: Optional[Tuple[int, int, int]]
    end: Optional[Tuple[int, int, int]]
    obs_time: Optional[str]  # raw stripped TIME_OF_OBS, None if sentinel/blank


def _parse_yyyymmdd(s: str) -> Optional[Tuple[int, int, int]]:
    s = s.strip()
    if len(s) != 8 or not s.isdigit():
        return None
    y, m, d = int(s[0:4]), int(s[4:6]), int(s[6:8])
    if y == 0 or y == 9999 and m == 99:
        return None
    return (y, m, d)


def _opt_float(s: str) -> Optional[float]:
    s = s.strip()
    if not s:
        return None
    try:
        return float(s)
    except ValueError:
        return None


def _zip_lines(zip_path: Path):
    with zipfile.ZipFile(zip_path) as zf:
        names = [n for n in zf.namelist() if not n.endswith("/")]
        for name in names:
            with zf.open(name) as fh:
                for raw in fh:
                    yield raw.decode("latin-1").rstrip("\r\n")


def read_mshr(zip_path: Path, wanted_ids: Set[str]) -> Dict[str, List[MshrRec]]:
    out: Dict[str, List[MshrRec]] = {}
    for line in _zip_lines(zip_path):
        if len(line) < 1414:
            continue
        sid = line[239:259].strip()
        if sid not in wanted_ids:
            continue
        rec = MshrRec(
            station_id=sid,
            begin=_parse_yyyymmdd(line[32:40]),
            end=_parse_yyyymmdd(line[41:49]),
            lat=_opt_float(line[1299:1319]),
            lon=_opt_float(line[1320:1340]),
            elev_ft=_opt_float(line[989:1029]),
            relocation=line[1352:1414].strip(),
        )
        out.setdefault(sid, []).append(rec)
    for sid in out:
        out[sid].sort(key=lambda r: (r.begin or (0, 0, 0), r.end or (0, 0, 0)))
    return out


def read_phr(zip_path: Path, wanted_ids: Set[str]) -> Dict[str, List[PhrRec]]:
    out: Dict[str, List[PhrRec]] = {}
    for line in _zip_lines(zip_path):
        if len(line) < 205:
            continue
        sid = line[85:105].strip()
        if sid not in wanted_ids:
            continue
        if line[124:134].strip() != "TEMP":
            continue
        if line[135:165].strip() != "COOP SOD":
            continue
        obs = line[197:205].strip() or None
        if obs is not None and obs[:4] in PHR_OBS_SENTINELS:
            obs = None
        rec = PhrRec(
            station_id=sid,
            begin=_parse_yyyymmdd(line[106:114]),
            end=_parse_yyyymmdd(line[115:123]),
            obs_time=obs,
        )
        out.setdefault(sid, []).append(rec)
    for sid in out:
        out[sid].sort(key=lambda r: (r.begin or (0, 0, 0), r.end or (0, 0, 0)))
    return out


# ---------------------------------------------------------------------------
# DMS quantization
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class DmsCoord:
    lat_d: int
    lat_m: int
    lat_s: int
    lon_d: int  # negative for west
    lon_m: int  # positive
    lon_s: int  # positive
    qlat: float
    qlon: float


def dms_quantize(lat: float, lon: float) -> DmsCoord:
    """Quantize to nearest arc-second, matching what a .his row can store.

    Negative (west/south) coordinates: negative degrees, positive
    minutes/seconds, and value = deg - min/60 - sec/3600 (the TOBMain
    his_lon formula).  Values in (-1, 0) cannot be represented (the sign
    lives on the degree field) - fail loud; no real station sits there.
    """
    if -1.0 < lon < 0.0:
        raise ValueError(f"longitude {lon} in (-1,0): sign not representable")
    if -1.0 < lat < 0.0:
        raise ValueError(f"latitude {lat} in (-1,0): sign not representable")

    south = lat < 0.0
    tot = int((abs(lat) * 3600.0) + 0.5)
    lat_ad, rem = divmod(tot, 3600)
    lat_m, lat_s = divmod(rem, 60)
    lat_d = -lat_ad if south else lat_ad

    west = lon < 0.0
    tot = int((abs(lon) * 3600.0) + 0.5)
    lon_ad, rem = divmod(tot, 3600)
    lon_m, lon_s = divmod(rem, 60)
    lon_d = -lon_ad if west else lon_ad

    if south:
        qlat = lat_d - lat_m / 60.0 - lat_s / 3600.0
    else:
        qlat = lat_d + lat_m / 60.0 + lat_s / 3600.0
    if west:
        qlon = lon_d - lon_m / 60.0 - lon_s / 3600.0
    else:
        qlon = lon_d + lon_m / 60.0 + lon_s / 3600.0
    return DmsCoord(lat_d, lat_m, lat_s, lon_d, lon_m, lon_s, qlat, qlon)


# ---------------------------------------------------------------------------
# .his rows
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class HisRow:
    source: int
    station_id: str
    beg: Tuple[int, int, int]
    end: Tuple[int, int, int]
    lat_d: float
    lat_m: float
    lat_s: float
    lon_d: float
    lon_m: float
    lon_s: float
    distdir: str
    elev: int
    instr_height: str
    obs_time_raw: str
    obs_code: int
    instruments: Tuple[str, ...]

    @property
    def lat_tob(self) -> float:
        return self.lat_d + self.lat_m / 60.0 + self.lat_s / 3600.0

    @property
    def lon_tob(self) -> float:
        # TOBMain.f95:497-504: his_lon = deg - min/60 - sec/3600
        return self.lon_d - self.lon_m / 60.0 - self.lon_s / 3600.0

    @property
    def lat_pha(self) -> float:
        # ReadInputFiles.f95:600: deg + (min + sec/60)/60 (sign NOT handled)
        return self.lat_d + (self.lat_m + self.lat_s / 60.0) / 60.0

    @property
    def lon_pha(self) -> float:
        return self.lon_d + (self.lon_m + self.lon_s / 60.0) / 60.0


def build_his_row(
    source: int,
    station_id: str,
    beg: Tuple[int, int, int],
    end: Tuple[int, int, int],
    dms: Optional[DmsCoord],
    elev_ft: int,
    obs_time: str,
    distdir: str = "",
    instr_height: str = "",
    instruments: Optional[List[str]] = None,
) -> str:
    """Emit one 152-char .his row per the FORMAT 90 column map above."""
    if not (0 <= source <= 9):
        raise ValueError(f"source {source} out of range")
    if len(station_id) > 12:
        raise ValueError(f"station id too long: {station_id!r}")
    if len(distdir) > 11:
        raise ValueError(f"distdir too long: {distdir!r}")
    if len(obs_time) > 4:
        raise ValueError(f"obs_time too long: {obs_time!r}")
    if len(instr_height) > 4:
        raise ValueError(f"instr_height too long: {instr_height!r}")
    instruments = instruments or []
    if len(instruments) > 11:
        raise ValueError("more than 11 instruments")
    if any(len(i) > 5 for i in instruments):
        raise ValueError("instrument code longer than 5 chars")

    if dms is None:
        lat_f = " " * 9
        lon_f = " " * 10
    else:
        lat_f = f"{dms.lat_d:3d}{dms.lat_m:3d}{dms.lat_s:3d}"
        lon_f = f"{dms.lon_d:4d}{dms.lon_m:3d}{dms.lon_s:3d}"

    row = (
        f"{source:1d}"  # col 1
        f"{station_id:<12s}"  # cols 2-13
        f" {beg[0]:04d}{beg[1]:02d}{beg[2]:02d}"  # cols 14-22
        f" {end[0]:04d}{end[1]:02d}{end[2]:02d}"  # cols 23-31
        f" {lat_f}"  # cols 32-41
        f" {lon_f}"  # cols 42-52
        f" {distdir:<11s}"  # cols 53-64
        f" {elev_ft:5d}"  # cols 65-70
        f"  {instr_height:<4s}"  # cols 71-76
        f" {obs_time:<4s}"  # cols 77-81
        f"{'':5s}"  # cols 82-86
    )
    instr_f = "".join(f"{i:<5s} " for i in instruments)
    row += f"{instr_f:<66s}"  # cols 87-152
    assert len(row) == HIS_ROW_WIDTH, len(row)
    return row


def parse_his_row(line: str, fill_end_year: Optional[int] = None) -> HisRow:
    """Parse one .his row with Fortran READ semantics (blank fields read 0).

    fill_end_year: replicate ReadInputFiles.f95:646-655 missing-date fills;
    when the end date is 99/99/9999 it becomes 12/31/<fill_end_year> if given
    (else stays 9999 with month/day set to 12/31).
    """
    line = f"{line:<{HIS_ROW_WIDTH}s}"
    beg_y = _fint(line[14:18])
    beg_m = _fint(line[18:20])
    beg_d = _fint(line[20:22])
    end_y = _fint(line[23:27])
    end_m = _fint(line[27:29])
    end_d = _fint(line[29:31])

    if beg_m == 99:
        beg_m = 6
    if beg_d == 99:
        beg_d = 15
    if end_m == 99 and end_d == 99 and end_y == 9999:
        end_m, end_d = 12, 31
        if fill_end_year is not None:
            end_y = fill_end_year
    else:
        if end_m == 99:
            end_m = 6
        if end_d == 99:
            end_d = 15

    obs_raw = line[77:81]
    instruments = tuple(line[86 + 6 * i : 91 + 6 * i].strip() for i in range(11))
    return HisRow(
        source=_fint(line[0:1]),
        station_id=line[1:13].strip(),
        beg=(beg_y, beg_m, beg_d),
        end=(end_y, end_m, end_d),
        lat_d=_ffloat(line[32:35]),
        lat_m=_ffloat(line[35:38]),
        lat_s=_ffloat(line[38:41]),
        lon_d=_ffloat(line[42:46]),
        lon_m=_ffloat(line[46:49]),
        lon_s=_ffloat(line[49:52]),
        distdir=line[53:64].rstrip(),
        elev=_fint(line[65:70]),
        instr_height=line[72:76].rstrip(),
        obs_time_raw=obs_raw,
        obs_code=decode_obtime(obs_raw),
        instruments=instruments,
    )


# ---------------------------------------------------------------------------
# Properties files
# ---------------------------------------------------------------------------


def write_properties(path: Path, props: Dict[str, str]) -> None:
    with open(path, "w", encoding="ascii") as fh:
        for key, value in props.items():
            fh.write(f"{key} = {value}\n")
