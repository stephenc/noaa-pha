#!/usr/bin/env python3
"""Emit reconstructed .his files.

One row per observation-time regime, explicit obs time on EVERY row (never
rely on blank-inherits-previous), all rows carrying the single chosen station
coordinate (TOBMain uses one coordinate per station; our runs use
tob.use-his-lat-lon=false so station.inv drives TOB - the .his coordinates
serve PHA and human review).

PHA reads the same rows: an obs-time-only change does NOT create a PHA
changepoint (ReadInputFiles.f95:723-728), so consecutive rows must keep every
other field identical unless a documented change is being mapped on purpose.
dist/dir stays blank by default because a non-blank value forces a PHA
changepoint (ReadInputFiles.f95:766-770).

Sunset-leakage invariant (TOBMain.f95:718-755, 809-821): tob_apply_year is
the begin YEAR of the first resolvable .his row, and months of that first
year BEFORE the first row's begin date are adjusted with the pre-history
default code 28 (sunset).  To keep that default from ever being applied, the
first emitted row must begin on (or before) Jan 1 of the station's first data
year.  emit_station_his enforces this by prepending an explicit 00HR row
(00HR decodes to 24 = midnight = zero adjustment, and it counts as
"resolvable", so it sets tob_apply_year without applying any adjustment).

Verbatim-field invariant (ReadInputFiles.f95:731-762, 793-795): PHA compares
elevation (integer) and instrument height / instrument codes (strings)
row-to-row; ANY difference injects a documented PHA changepoint.  Every row
emitted here therefore repeats those fields byte-identically.
"""

from __future__ import annotations

import warnings
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple

import ghcn_io
from ghcn_io import DmsCoord, HisRow, Inv, MshrRec

MAX_CODE_CHANGES = 200  # TOBMain MAX_CHANGES

_DAYS = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)


def _leap(y: int) -> bool:
    return y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)


def _days_in_month(y: int, m: int) -> int:
    if m == 2 and _leap(y):
        return 29
    return _DAYS[m - 1]


def _day_before(date: Tuple[int, int, int]) -> Tuple[int, int, int]:
    y, m, d = date
    if d > 1:
        return (y, m, d - 1)
    if m > 1:
        return (y, m - 1, _days_in_month(y, m - 1))
    return (y - 1, 12, 31)


@dataclass(frozen=True)
class Regime:
    begin: Tuple[int, int, int]
    obs_time: str
    end: Optional[Tuple[int, int, int]] = None


def emit_station_his(
    station_id: str,
    regimes: List[Regime],
    coord_dms: DmsCoord,
    inv: Inv,
    out_dir: Path,
    first_data_year: int,
    mshr: Optional[List[MshrRec]] = None,
    source: int = 0,
) -> Path:
    """Write <station_id>.his to out_dir and return the path.

    Rows are one per regime; each regime's end is the day before the next
    regime's begin (an explicit Regime.end overrides); the final row ends
    9999/12/31 so the timeline covers all data.

    first_data_year: the station's first year of data.  If the first regime
    begins after Jan 1 of that year, an explicit 00HR row (zero adjustment,
    see module docstring) is prepended spanning (first_data_year, 1, 1) to
    the day before the first regime's begin, so TOBMain's pre-history sunset
    default (code 28) can never leak into adjusted months.

    If mshr is given, rows are additionally split at documented MSHR moves
    (records with a non-blank RELOCATION whose begin falls strictly inside a
    regime).  Split rows keep the regime's obs time and the single chosen
    coordinate; the RELOCATION text (truncated to 11 chars) goes in dist/dir
    ONLY on the row beginning exactly at the move - that is what tells PHA a
    documented changepoint happened there.
    """
    if not regimes:
        raise ValueError("no regimes to emit")
    begins = [r.begin for r in regimes]
    if begins != sorted(begins):
        raise ValueError(f"{station_id}: regimes not chronological")

    if regimes[0].begin > (first_data_year, 1, 1):
        regimes = [Regime(begin=(first_data_year, 1, 1), obs_time="00HR")] + list(
            regimes
        )

    if inv.elev_m is not None:
        elev_ft = int(round(inv.elev_m * 3.28084))
    else:
        elev_ft = 0

    # (begin, obs_time, distdir) triples after optional MSHR splitting
    rows: List[Tuple[Tuple[int, int, int], str, str]] = []
    moves = []
    if mshr:
        for rec in mshr:
            if rec.relocation and rec.begin is not None:
                moves.append((rec.begin, rec.relocation[:11]))
        moves.sort()
    for i, reg in enumerate(regimes):
        nxt = regimes[i + 1].begin if i + 1 < len(regimes) else None
        rows.append((reg.begin, reg.obs_time, ""))
        for mv_begin, mv_text in moves:
            if mv_begin > reg.begin and (nxt is None or mv_begin < nxt):
                rows.append((mv_begin, reg.obs_time, mv_text))

    out_dir.mkdir(parents=True, exist_ok=True)
    path = out_dir / f"{station_id}.his"
    with open(path, "w", encoding="ascii") as fh:
        for i, (begin, obs_time, distdir) in enumerate(rows):
            if i + 1 < len(rows):
                end = _day_before(rows[i + 1][0])
            else:
                end = (9999, 12, 31)
            explicit = regimes[-1].end if i + 1 == len(rows) else None
            if explicit is not None:
                end = explicit
            fh.write(
                ghcn_io.build_his_row(
                    source=source,
                    station_id=station_id,
                    beg=begin,
                    end=end,
                    dms=coord_dms,
                    elev_ft=elev_ft,
                    obs_time=obs_time,
                    distdir=distdir,
                )
                + "\n"
            )
    return path


def validate_his_file(
    path: Path,
    first_data_year: Optional[int] = None,
    warnings_out: Optional[List[str]] = None,
) -> List[HisRow]:
    """Parse and sanity-check an emitted .his file.

    Raises ValueError on: wrong line width, non-chronological begin dates,
    unknown obs code on a non-blank field, or more than MAX_CODE_CHANGES
    decoded code changes (TOBMain caps at 200).

    Warnings (appended to warnings_out if given, else emitted via
    warnings.warn) - conditions that do not break TOBMain but change
    behavior in ways an emitted file must normally avoid:
      (a) first row begins after Jan 1 of first_data_year (pre-history
          sunset default would be applied to earlier months of that year);
      (b) row-to-row change in elevation, instrument height, or instrument
          codes (injects a documented PHA changepoint);
      (c) non-blank dist/dir (injects a documented PHA changepoint).
    """
    rows: List[HisRow] = []
    with open(path, "r", encoding="ascii") as fh:
        for lineno, line in enumerate(fh, 1):
            line = line.rstrip("\n")
            if len(line) != ghcn_io.HIS_ROW_WIDTH:
                raise ValueError(
                    f"{path}:{lineno}: width {len(line)} != " f"{ghcn_io.HIS_ROW_WIDTH}"
                )
            rows.append(ghcn_io.parse_his_row(line))

    prev = None
    for i, row in enumerate(rows):
        if prev is not None and row.beg < prev:
            raise ValueError(
                f"{path}: row {i + 1} begin {row.beg} before " f"previous begin {prev}"
            )
        prev = row.beg
        if row.obs_time_raw.strip() and row.obs_code == 99:
            raise ValueError(
                f"{path}: row {i + 1} unknown obs time " f"{row.obs_time_raw!r}"
            )

    changes = 0
    prev_code = None
    for row in rows:
        if prev_code is None or row.obs_code != prev_code:
            changes += 1
        prev_code = row.obs_code
    if changes > MAX_CODE_CHANGES:
        raise ValueError(f"{path}: {changes} code changes > {MAX_CODE_CHANGES}")

    def warn(msg: str) -> None:
        if warnings_out is not None:
            warnings_out.append(msg)
        else:
            warnings.warn(msg, stacklevel=2)

    if rows:
        if first_data_year is not None and rows[0].beg > (first_data_year, 1, 1):
            warn(
                f"{path}: first row begins {rows[0].beg} after Jan 1 "
                f"{first_data_year}; pre-history sunset default would apply"
            )
        for i in range(1, len(rows)):
            a, b = rows[i - 1], rows[i]
            if (
                a.elev != b.elev
                or a.instr_height != b.instr_height
                or a.instruments != b.instruments
            ):
                warn(
                    f"{path}: row {i + 1} changes elevation/instrument "
                    f"fields (documented PHA changepoint injected)"
                )
        for i, row in enumerate(rows):
            if row.distdir:
                warn(
                    f"{path}: row {i + 1} non-blank dist/dir "
                    f"{row.distdir!r} (documented PHA changepoint injected)"
                )
    return rows


def emit_metadata_his(
    station_id: str,
    phr_recs: Optional[List],
    mshr_recs: Optional[List],
    inv: Inv,
    out_dir: Path,
    source: int = 0,
    first_data: Optional[Tuple[int, int]] = None,
) -> Optional[Path]:
    """Metadata-derived .his for stations outside the TOB gate (no TOB solve).

    Builds documented history rows from MSHR-enhanced location/elevation
    periods merged with PHR period boundaries.  PHA's reader derives
    documented changepoints from row-to-row changes (lat/lon jumps >= 45
    arc-sec, elevation changes, non-blank dist/dir); the obs-time field
    never creates a changepoint, and TOBMain verbatim-copies stations
    outside the CONUS gate.  NOAA applied no TOB adjustment to these
    stations, so every row carries the no-TOB code 24HR (zero adjustment):
    explicit and safe in any TOB context, and immune to the blank-obs rule
    that would otherwise inherit a previous code or the pre-history sunset
    default.

    Every row uses source 0: PHA honors source-0 rows unconditionally,
    whereas source-2 (MSHR) rows are consumed only when they begin after
    the last source-0/3 row, so an all-source-2 file would lean on that
    reader rule's edge behavior.

    dist/dir carries the documented RELOCATION text (11 chars) only on the
    row beginning exactly at the relocating MSHR period -- injecting the
    documented PHA changepoint there and nowhere else.  Rows where nothing
    .his-visible changes are merged.  MSHR carries international GHCN-D ids
    too; their files are equally valid (negative degrees encode south/west),
    and TOBMain's CONUS gate guarantees no TOB adjustment regardless.

    first_data, when given, is the (year, month) of the earliest value
    present in the raw QCU record (QC-flagged months count as data); the
    emitted history is clamped to start there -- metadata periods that
    begin earlier (some MSHR records carry year-1 begin dates) collapse
    into a first row beginning on the first day of that month, carrying
    the metadata state active at that date.  A relocation marker on a
    clamped-away boundary is dropped: the move predates the data record.
    Returns the written path, or None when the station has no dated HOMR
    records.
    """
    mshr = sorted(
        (r for r in (mshr_recs or []) if r.begin is not None),
        key=lambda r: r.begin,
    )
    phr = sorted(
        (r for r in (phr_recs or []) if r.begin is not None),
        key=lambda r: r.begin,
    )
    boundaries = sorted({r.begin for r in mshr} | {r.begin for r in phr})
    if not boundaries:
        return None

    def active(recs, date):
        cur = None
        for r in recs:
            if r.begin <= date:
                cur = r
            else:
                break
        return cur

    inv_elev_ft = int(round(inv.elev_m * 3.28084)) if inv.elev_m is not None else 0

    rows = []  # (begin, obs_code, dms, elev_ft, distdir)
    for date in boundaries:
        m = active(mshr, date)
        obs = "24HR"
        if m is not None and m.lat is not None and m.lon is not None:
            dms = ghcn_io.dms_quantize(m.lat, m.lon)
        else:
            dms = ghcn_io.dms_quantize(inv.lat, inv.lon)
        elev_ft = (
            int(round(m.elev_ft))
            if (m is not None and m.elev_ft is not None)
            else inv_elev_ft
        )
        distdir = ""
        if m is not None and m.begin == date and m.relocation:
            distdir = m.relocation[:11]
        if rows and not distdir:
            _pb, pobs, pdms, pelev, _pd = rows[-1]
            if (pobs, pdms, pelev) == (obs, dms, elev_ft):
                continue  # nothing .his-visible changes at this boundary
        rows.append((date, obs, dms, elev_ft, distdir))

    if not rows:
        return None
    if first_data is not None:
        clamp = (first_data[0], first_data[1], 1)
        first_kept = next((i for i, r in enumerate(rows) if r[0] >= clamp), len(rows))
        if first_kept > 0:
            if first_kept < len(rows) and rows[first_kept][0] == clamp:
                rows = rows[first_kept:]
            else:
                _b, obs, dms, elev_ft, _d = rows[first_kept - 1]
                rows = [(clamp, obs, dms, elev_ft, "")] + rows[first_kept:]
    out_dir.mkdir(parents=True, exist_ok=True)
    path = out_dir / f"{station_id}.his"
    with open(path, "w", encoding="ascii") as fh:
        for i, (begin, obs, dms, elev_ft, distdir) in enumerate(rows):
            end = _day_before(rows[i + 1][0]) if i + 1 < len(rows) else (9999, 12, 31)
            fh.write(
                ghcn_io.build_his_row(
                    source=source,
                    station_id=station_id,
                    beg=begin,
                    end=end,
                    dms=dms,
                    elev_ft=elev_ft,
                    obs_time=obs,
                    distdir=distdir,
                )
                + "\n"
            )
    return path
