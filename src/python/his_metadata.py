#!/usr/bin/env python3
"""Metadata-complete `.his` emission: evidence for TOB, HOMR for everything else.

## The problem this solves

A reconstructed history is evidence-based for the observation time: the residual
solve, donor hints, then PHR (see ``phr_fill``).  For every *other* `.his` field
``his_emit.emit_station_his`` deliberately writes a constant -- one coordinate,
one elevation, blank dist/dir, blank instruments -- the "verbatim-field
invariant".  That is correct for a TOB-only pipeline, because any row-to-row
change in those fields injects a documented PHA changepoint, and we did not want
to invent one.

The cost is invisible until PHA reads the histories.  ``ReadInputFiles.f95``
records a documented changepoint only when ``history_code /= 0``, and an
observation-time change never sets it -- it merely appends ``' OBT'`` to the
diagnostic string (f95 lines 722-729).  ``history_code`` is set by:

  * instrument height change            (``IHT``)
  * instrument change                   (``INST`` / ``ASOS``)
  * non-blank distance/direction        (``LDIS``)
  * elevation / latitude / longitude    (``ELEV`` / ``LALO`` -> ``MOVE``)

So a flattened history yields **no** documented changepoints, and running PHA
with ``pha.use-history-files = 1`` over it is a no-op by construction.  NOAA's
real histories carry all of the above.

This module rebuilds the non-TOB fields from the metadata NOAA publishes, while
leaving the observation-time timeline exactly as the evidence established it.

## Field provenance

| `.his` field        | source                              | spec |
|---------------------|-------------------------------------|------|
| observation time    | residual solve > hints > PHR        | (ours) |
| latitude/longitude  | MSHR ``LAT_DEC`` / ``LON_DEC``      | MSHR_Enhanced_Table.txt 1300-1319 / 1321-1340 |
| elevation           | MSHR ``ELEV_GROUND``                | MSHR_Enhanced_Table.txt 990-1029 |
| distance/direction  | MSHR ``RELOCATION``                 | MSHR_Enhanced_Table.txt 1353-1414 |
| instruments         | PHR ``EQUIPMENT``                   | PHR_Table.txt 207-216 |
| instrument height   | **not published by HOMR** -> blank  | -- |

Instrument height is left blank deliberately: no HOMR report carries it, and
PHA treats a blank as "unknown" and will not fire a move on it.  Inventing a
value would manufacture changepoints.

## Why the TOB output is unaffected

TOBMain reads the observation time and (with ``tob.use-his-lat-lon = false``,
which these runs use) takes coordinates from ``station.inv``, not the `.his`.
It reads neither elevation, instruments nor dist/dir.  The obs-time timeline
here is copied verbatim from the evidence-based file, so the decoded code
sequence is identical and the TOB series is byte-for-byte unchanged.  Callers
should nonetheless *verify* that rather than trust it -- ``--verify-tob-dir``
exists for exactly that, and the CLI refuses to claim success without it.

## Reproducing the inputs

``fetch_homr.py`` downloads MSHR and PHR (and their official layout specs) into
a base directory and records URL, timestamp and SHA-256 for each, so a future
run can prove which vintage of the metadata it used.
"""

from __future__ import annotations

import argparse
import os
import sys
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import ghcn_io  # noqa: E402
import his_emit  # noqa: E402

Date = Tuple[int, int, int]

# PHR EQUIPMENT -> the 5-char token PHA matches in `instr_strings`
# (ReadInputFiles.f95:487-491).  Values absent from PHA's vocabulary map to
# None and are emitted as *no* instrument, which reads as "unknown" and cannot
# fire a spurious move.  Keep this table explicit: silently passing an
# unrecognised token through would land in the instrument columns and be
# compared against PHA's list as garbage.
PHR_EQUIPMENT_TO_PHA: Dict[str, Optional[str]] = {
    "MMTS": "MMTS",
    "NIMBUS": "NIMBS",  # PHA spells it NIMBS
    "MXMN": "MXMN",
    "HYGR": "HYGR",
    "PSY": "PSY",
    "CRS": "CRS",
    "TG": "TG",
    "ASOS": "ASOS",
    "AMOS": "AMOS",
    "TELSY": "TELSY",
    "DGT": "DGT",
    "SIX-T": None,  # not in PHA's vocabulary
    "ATEMP": None,
    "TEMPX": None,
    "HTG": None,
    "THERM-NWS": None,
    "SOLRX": None,
    "MISCX": None,
    "PYR": None,
    "UNKNOWN": None,
    "": None,
}


def map_equipment(raw: str) -> Optional[str]:
    """PHA instrument token for a raw PHR EQUIPMENT value, or None if unknown."""
    return PHR_EQUIPMENT_TO_PHA.get((raw or "").strip().upper())


@dataclass(frozen=True)
class RowState:
    """Everything a `.his` row carries besides its begin/end dates."""

    obs_time: str
    dms: Optional[ghcn_io.DmsCoord]
    elev_ft: int
    instruments: Tuple[str, ...]

    def visible_key(self):
        return (self.obs_time, self.dms, self.elev_ft, self.instruments)


# PHA's own move threshold for coordinates: ReadInputFiles.f95:496
#   real, parameter :: latlon_epsilon = 0.0125   ! degrees == 45 arcsec
LATLON_EPSILON = 0.0125


def _pha_visible_change(a: RowState, b: RowState) -> bool:
    """Would PHA see any difference between these two rows?

    Deliberately uses PHA's *own* comparison semantics rather than exact field
    equality:

      * observation time -- any change (TOBMain needs it, even though PHA does
        not treat it as a changepoint);
      * elevation and instruments -- exact, which is how PHA compares them;
      * latitude/longitude -- only beyond ``latlon_epsilon``.  A sub-threshold
        re-survey nudge cannot produce a changepoint, so emitting a row for it
        would add a row that changes nothing.
    """
    if a.obs_time != b.obs_time:
        return True
    if a.elev_ft != b.elev_ft:
        return True
    if a.instruments != b.instruments:
        return True
    if (a.dms is None) != (b.dms is None):
        return True
    if a.dms is not None and b.dms is not None:
        if (
            abs(b.dms.qlat - a.dms.qlat) > LATLON_EPSILON
            or abs(b.dms.qlon - a.dms.qlon) > LATLON_EPSILON
        ):
            return True
    return False


def _active(recs: Sequence, date: Date):
    """Latest record whose begin is on/before `date` and which has not ended."""
    best = None
    for r in recs:
        if r.begin is None or r.begin > date:
            continue
        if r.end is not None and r.end < date:
            continue
        if best is None or r.begin >= best.begin:
            best = r
    return best


def read_obs_timeline(path: Path) -> List[his_emit.Regime]:
    """Observation-time regimes from an already-emitted `.his`.

    Taking the timeline from the emitted file (rather than re-deriving it)
    guarantees this is *exactly* the shipped evidence-based history plus
    metadata -- there is no second code path that could disagree.
    """
    out: List[his_emit.Regime] = []
    with open(path, "r", encoding="ascii") as fh:
        for line in fh:
            line = line.rstrip("\n")
            if len(line) != ghcn_io.HIS_ROW_WIDTH:
                continue
            row = ghcn_io.parse_his_row(line)
            obs = row.obs_time_raw.strip()
            if not obs:
                continue
            if not out or out[-1].obs_time != obs:
                out.append(his_emit.Regime(begin=row.beg, obs_time=obs))
    return out


def build_rows(
    regimes: Sequence[his_emit.Regime],
    mshr: Sequence,
    phr: Sequence,
    inv: ghcn_io.Inv,
    stats: Optional[Counter] = None,
) -> List[Tuple[Date, RowState, str]]:
    """Merge the obs-time timeline with MSHR/PHR metadata into `.his` rows.

    Returns ``(begin_date, state, distdir)`` triples.  Rows at which nothing
    `.his`-visible changes are dropped, except a row carrying a relocation
    marker (that marker *is* the visible change).

    The first obs-time regime's begin date is the floor: metadata boundaries
    earlier than the history's own start are folded into the first row, so
    TOBMain's ``tob_apply_year`` (the begin year of the first resolvable row)
    is unchanged and the pre-history sunset default still cannot leak in.
    """
    st = stats if stats is not None else Counter()
    if not regimes:
        return []
    floor = regimes[0].begin
    inv_elev = (
        his_emit._nint(inv.elev_m * his_emit._FT_PER_M) if inv.elev_m is not None else 0
    )
    inv_dms = ghcn_io.dms_quantize(inv.lat, inv.lon)

    boundaries = {r.begin for r in regimes}
    for r in mshr:
        if r.begin is not None and r.begin > floor:
            boundaries.add(r.begin)
    for r in phr:
        if r.begin is not None and r.begin > floor:
            boundaries.add(r.begin)
    dates = sorted(d for d in boundaries if d >= floor)

    obs_at = {r.begin: r.obs_time for r in regimes}
    cur_obs = regimes[0].obs_time

    out: List[Tuple[Date, RowState, str]] = []
    last: Optional[RowState] = None
    last_instr: Tuple[str, ...] = ()
    for d in dates:
        if d in obs_at:
            cur_obs = obs_at[d]
        m = _active(mshr, d)
        if m is not None and m.lat is not None and m.lon is not None:
            dms = ghcn_io.dms_quantize(m.lat, m.lon)
        else:
            dms = inv_dms
        elev = (
            his_emit._nint(m.elev_ft)
            if (m is not None and m.elev_ft is not None)
            else inv_elev
        )
        p = _active(phr, d)
        tok = map_equipment(p.equipment) if p is not None else None
        if tok is not None:
            instruments: Tuple[str, ...] = (tok,)
            last_instr = instruments
        else:
            # Undocumented equipment is NOT an instrument change.  PHR is full
            # of UNKNOWN/ATEMP/blank values; emitting "no instrument" there
            # would flip instr(21)/instr(2) against the previous row and make
            # PHA read a MOVE out of a *gap in documentation*.  Carry the last
            # known instrument forward instead.
            instruments = last_instr
            if p is not None and p.equipment:
                st["equipment_unmapped"] += 1

        distdir = ""
        for r in mshr:
            if r.begin == d and r.relocation:
                distdir = r.relocation[:11]
                break

        state = RowState(cur_obs, dms, elev, instruments)
        if last is not None and not distdir and not _pha_visible_change(last, state):
            # Nothing PHA can act on differs from the LAST EMITTED row.  Compare
            # against that rather than the previous candidate, so a slow drift of
            # sub-threshold nudges still emits once it accumulates past the
            # threshold.
            st["row_suppressed_no_visible_change"] += 1
            continue
        out.append((d, state, distdir))
        last = state

    # Attribute what changed, for the report.  The marginal counters matter:
    # PHA compares elevation with `/=` and lat/lon against latlon_epsilon
    # (0.0125 deg == 45 arcsec, ReadInputFiles.f95:496), so a one-foot re-survey
    # becomes a MOVE while a sub-epsilon coordinate nudge does not.  Counting
    # both tells us how many documented changepoints rest on trivia.
    for i in range(1, len(out)):
        a, b = out[i - 1][1], out[i][1]
        if b.dms != a.dms:
            st["change_latlon"] += 1
            if a.dms is not None and b.dms is not None:
                if (
                    abs(b.dms.qlat - a.dms.qlat) <= 0.0125
                    and abs(b.dms.qlon - a.dms.qlon) <= 0.0125
                ):
                    st["change_latlon_below_pha_epsilon"] += 1
        if b.elev_ft != a.elev_ft:
            st["change_elevation"] += 1
            if abs(b.elev_ft - a.elev_ft) <= 1:
                st["change_elevation_within_1ft"] += 1
        if b.instruments != a.instruments:
            st["change_instruments"] += 1
        if out[i][2]:
            st["change_relocation"] += 1
        if b.obs_time != a.obs_time:
            st["change_obs_time"] += 1
    return out


def write_rows(
    station_id: str, rows: Sequence[Tuple[Date, RowState, str]], out_dir: Path
) -> Path:
    out_dir.mkdir(parents=True, exist_ok=True)
    path = out_dir / f"{station_id}.his"
    with open(path, "w", encoding="ascii") as fh:
        for i, (begin, state, distdir) in enumerate(rows):
            end = (
                his_emit._day_before(rows[i + 1][0])
                if i + 1 < len(rows)
                else (9999, 12, 31)
            )
            fh.write(
                ghcn_io.build_his_row(
                    source=0,
                    station_id=station_id,
                    beg=begin,
                    end=end,
                    dms=state.dms,
                    elev_ft=state.elev_ft,
                    obs_time=state.obs_time,
                    distdir=distdir,
                    instruments=list(state.instruments),
                )
                + "\n"
            )
    return path


def main(argv: Optional[List[str]] = None) -> int:
    ap = argparse.ArgumentParser(
        description="Rewrite emitted .his files with HOMR metadata in the "
        "non-TOB fields, leaving the observation-time timeline untouched."
    )
    ap.add_argument("--base", required=True, help="base whose histories to rewrite")
    ap.add_argument(
        "--mshr-zip", default=None, help="default <base>/mshr_enhanced.txt.zip"
    )
    ap.add_argument("--phr-zip", default=None, help="default <base>/phr.txt.zip")
    ap.add_argument(
        "--conus-only",
        action="store_true",
        default=True,
        help="restrict to the CONUS TOB gate (default; non-CONUS histories are "
        "already metadata-derived by his_emit.emit_metadata_his)",
    )
    ap.add_argument("--report", default=None)
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args(argv)

    base = Path(args.base)
    his_dir = base / "intermediate" / "history"
    inv_path = base / "intermediate" / "station.inv"
    mshr_zip = Path(args.mshr_zip) if args.mshr_zip else base / "mshr_enhanced.txt.zip"
    phr_zip = Path(args.phr_zip) if args.phr_zip else base / "phr.txt.zip"
    for p in (his_dir, inv_path, mshr_zip, phr_zip):
        if not p.exists():
            sys.exit("missing: %s" % p)

    from phr_fill import is_conus

    inv = ghcn_io.read_inventory(inv_path)
    sids = sorted(p.name[: -len(".his")] for p in his_dir.glob("*.his"))
    if args.conus_only:
        sids = [s for s in sids if s in inv and is_conus(s, inv[s].lat, inv[s].lon)]
    mshr = ghcn_io.read_mshr(mshr_zip, set(sids))
    phr = ghcn_io.read_phr(phr_zip, set(sids))
    print(
        "# stations=%d  MSHR=%d  PHR=%d" % (len(sids), len(mshr), len(phr)), flush=True
    )

    stats: Counter = Counter()
    rows_out: List[str] = []
    n_written = 0
    for sid in sids:
        path = his_dir / f"{sid}.his"
        regimes = read_obs_timeline(path)
        if not regimes:
            stats["no_obs_timeline"] += 1
            continue
        m = mshr.get(sid) or []
        p = phr.get(sid) or []
        if not m and not p:
            stats["no_metadata"] += 1
            continue
        before = sum(1 for _ in open(path))
        rows = build_rows(regimes, m, p, inv[sid], stats)
        if not rows:
            stats["no_rows"] += 1
            continue
        if len(rows) > his_emit.MAX_CODE_CHANGES:
            # TOBMain caps decoded code changes; rows themselves are cheaper,
            # but stay conservative and skip rather than emit an invalid file.
            stats["refuse_too_many_rows"] += 1
            continue
        stats["stations_rewritten"] += 1
        stats["rows_before"] += before
        stats["rows_after"] += len(rows)
        rows_out.append("%s\t%d\t%d" % (sid, before, len(rows)))
        if not args.dry_run:
            write_rows(sid, rows, his_dir)
            n_written += 1

    out = ["# --- metadata-complete .his (dry_run=%s) ---" % args.dry_run]
    for k in sorted(stats):
        out.append("%s\t%d" % (k, stats[k]))
    text = "\n".join(out)
    print(text)
    print("# histories rewritten: %d" % n_written)
    if args.report:
        with open(args.report, "w") as fh:
            fh.write("station_id\trows_before\trows_after\n")
            for r in rows_out:
                fh.write(r + "\n")
        with open(args.report + ".summary", "w") as fh:
            fh.write(text + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
