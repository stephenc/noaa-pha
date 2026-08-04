# Python helpers

These scripts are additions to this repository. NOAA's source tarball does not
contain them. They do this work:

- Build the PHA input workspace from the published QCU dataset.
- Build comparable output files from the published QCF dataset.
- Recover the unpublished station history (`.his`) files. The CONUS histories
  come from the QCU/QCF residuals, and the others from HOMR metadata.
- Verify that a TOBMain run agrees with the solver solutions.
- Compare and score the output.

## What each script does

### Workspace prep

- `qcu_to_inputs.py`
  - Reads a GHCN-M v4 **QCU** `tar.gz` and builds the PHA input directory layout.
  - Converts fixed-width `TAVG` `.dat` into per-station files for the PHA pipeline.
  - Writes `raw.properties` and `tob.properties` (TOB stage under
    `intermediate/tob/tavg`; station history under `intermediate/history`).

- `qcf_to_outputs.py`
  - Reads a GHCN-M v4 **QCF** `tar.gz` and writes per-station output files.
  - Intended for like-for-like comparisons with PHA outputs.

- `compare_dirs.py`
  - Compares two directories of per-station files.
  - Matches by station ID (first filename segment).
  - Outputs summary stats in Celsius and 30-year bins.

### Station-history recovery (primary feature)

One command does all of the recovery. It reads QCU, QCF and the HOMR metadata,
and writes the `.his` files:

```bash
uv run python src/python/fetch_homr.py --base data        # metadata, once
uv run python src/python/reconstruct_his.py --base data
```

`--hints` reads a hint databank. The option takes one directory, and you can
give it more than once:

```bash
args=(); for d in /path/to/hintstore/*/; do args+=(--hints "$d"); done
uv run python src/python/reconstruct_his.py --base data "${args[@]}" --jobs 16
```

Four phases run in one process, thus there is one entry point and the caller
cannot get the sequence wrong:

1. **solve** — decompose the QCU/QCF residuals into observation-time regimes,
   and use the donor hints. This phase takes the most time. It writes a history
   only for the stations that NOAA is responsible for (`--no-us-responsible`).
2. **PHR fill** — document the months that no vintage constrains. The order of
   authority is: this vintage, then donor hints, then PHR
   (`--no-phr-fill`, `--phr-fill-regions pre,interior,post`).
3. **metadata rows** — write the other `.his` fields from HOMR
   (`--no-metadata-rows` gives a history with TOB fields only).
4. **COOP histories** — write metadata histories for the CONUS COOP stations
   that have no TOB solve (`--no-coop-history`). This phase must be last,
   because phase 2 and phase 3 must not change the files that it writes.

Phase 2 and phase 3 read only what the solve wrote. You can run them again on a
finished base, and they give the same result. Thus you can change their policy
without a new solve.

#### How to build a hint databank

Use the same command. Run it with no `--hints` option on each pair of QCU and
QCF files. Each run writes its evidence to `<base>/intermediate/hints`. Collect
those directories to make a databank. A vintage does not need a databank to
supply one, and a run refuses the hints from its own base.

#### What the recovery can and cannot prove

The residual evidence reaches as far as QCF does, and no further. A QCU month
with no QCF value has no residual to decompose. Thus this vintage cannot
establish the observation time for that month.

Vintages keep and remove different segments. Thus a month that one vintage
cannot reach can have a QCF value in another one. A databank imports what those
vintages proved. More vintages increase the fraction that is proven, but no
databank makes it complete.

PHR and HOMR document what no vintage proves. That is documentation, not
evidence. Do not describe the result as final.

- `reconstruct_his.py` — driver CLI (default: full inventory, emission on).
  - Classifies stations, solves CONUS residuals, emits `.his` into
    `<base>/intermediate/history`, and provisions `<base>/intermediate/`
    (`station.inv` filtered to QCF stations + empty `tob/tavg` out-dir).
  - Example: `uv run python src/python/reconstruct_his.py --base data`

- `fetch_homr.py` — downloads MSHR + PHR and their official layout specs into a
  base, writing `homr_provenance.json` (URL, UTC timestamp, size, SHA-256).
  HOMR publishes undated "latest" files, so recording which copy was used is
  what makes a reconstruction reproducible. `--check` re-verifies the hashes.

### Observation time: evidence first, metadata second

- `phr_fill.py` — writes PHR observation times into the months that the
  residual solve does not constrain. A mask of authority for each month
  controls this. The fill never changes a month inside a constrained run of
  this vintage, or a month that an adopted donor hint covers. The mask also
  keeps the bit-exact QCF identity, because a month with a new code can break
  `qcf == pha_qcf(t_out, S)`. The fill refuses a complete run if the run
  touches a month with a QCF value, or the start of a solved regime. It counts
  each refusal.

### Everything except observation time: `his_metadata.py`

`his_emit.emit_station_his` writes constant coordinates and elevation, and
leaves dist/dir and the instruments blank. This is the verbatim-field
invariant. It makes sure that a TOB-only pipeline adds no false PHA
changepoint.

There is a consequence. PHA with `pha.use-history-files = 1` finds **no
documented changepoints** in such a history. `ReadInputFiles.f95` sets
`history_code` from the instrument height, the instruments, dist/dir or a
change of position. It never sets it from a change of observation time.

`his_metadata.py` rebuilds those fields from what NOAA publishes:

| `.his` field       | source                         | spec |
|--------------------|--------------------------------|------|
| observation time   | solve > hints > PHR            | (ours) |
| latitude/longitude | MSHR `LAT_DEC` / `LON_DEC`     | MSHR_Enhanced_Table.txt 1300-1319 / 1321-1340 |
| elevation          | MSHR `ELEV_GROUND`             | MSHR_Enhanced_Table.txt 990-1029 |
| dist/dir           | MSHR `RELOCATION`              | MSHR_Enhanced_Table.txt 1353-1414 |
| instruments        | PHR `EQUIPMENT`                | PHR_Table.txt 207-216 |
| instrument height  | **not published by HOMR** → blank | — |

Two rules keep it from inventing changepoints:

- **A row is written only when PHA could see the difference.** Comparison uses
  PHA's own semantics against the *last emitted* row: exact for elevation and
  instruments, but coordinates only beyond `latlon_epsilon` (45 arcsec), since a
  sub-threshold re-survey nudge can never produce a changepoint. Comparing
  against the last emitted row (not the previous candidate) still catches a slow
  drift once it accumulates past the threshold.
- **Undocumented equipment is not an instrument change.** PHR is full of
  `UNKNOWN`/`ATEMP`/blank values; emitting "no instrument" there would flip
  `instr(21)` against the previous row and make PHA read a `MOVE` out of a gap
  in documentation. The last known instrument is carried forward instead.

`validate_his_file` will warn about elevation/instrument changes and non-blank
dist/dir for these files. That is expected: those warnings exist to police the
verbatim-field invariant, which this mode relaxes on purpose.

This does not change the TOB series. TOBMain reads the observation time. With
`tob.use-his-lat-lon=false` it takes the coordinates from `station.inv`. It
does not read the elevation, the instruments or dist/dir. Do not assume this.
Build the TOB again from a `--no-metadata-rows` history into a scratch
directory, and compare the two.

- `residual_solver.py` — exact decomposition of QCF−QCU residuals into TOB
  regime timelines + PHA segment sums (interval arithmetic; no tolerances).

- `tob_basis.py` — batched fake-inventory TOBMain harness producing exact
  per-(code, month, year) adjustment offsets for the solver.

- `fp32.py` — bit-exact float32 kernel and interval recovery matching TOBMain
  (value-dependent rounding, per-term blend division).

- `ghcn_io.py` — FORMAT-90 `.his` I/O plus GHCN / MSHR / PHR fixed-width readers.

- `his_emit.py` — regime-to-`.his` emission (PHA-safe field rules) and
  metadata-derived histories for non-CONUS stations.

- `metadata_accuracy.py` — scores PHR obs-time records against recovered
  timelines (analysis helper, not required for reconstruction).

- `tob_hints.py` — cross-vintage TOB **evidence hints**: capture, I/O,
  derivation, and consolidation (subsumes the former `merge_history.py`).

### Cross-vintage TOB hints (evidence capture + consolidation)

Each run writes a **hints** file for each station to
`<base>/intermediate/hints/<sid>.hints.json`. The file records what the
residual solve established: constraint runs, the coverage of each regime,
offset-ambiguity sets and boundary evidence (schema `tob-hints/1`). The
contents are a function of the residual solve alone. `--no-hints-out` stops
these files.

When another QCF vintage covers years that this base does not, consolidate its
hints into this base's timeline. Consolidation extends the timeline **outside
this vintage's QCF hull only**:

```bash
uv run python src/python/reconstruct_his.py --base data \
    --hints data-oldest/intermediate/hints
```

- **QCF hull is definitive** (first–last present QCF month, holes included);
  in-hull months always follow the current solve — hints never fill holes.
- **Pre-hull** `[first_qcu, qcf_first)` / **post-hull** `(qcf_last, last_qcu]`:
  adopt the **primary** donor's adoptable regimes (class `residual-proven`
  or `residual-ambiguous`, valid basis code, in-window begin).  Pad codes
  (`00HR`/`24HR`) participate in precedence and, on winning, emit an explicit
  `24HR` row.
- **Precedence** per window: evidence class, then run-aware coverage, then
  `--hints` order (later-listed wins full ties).  Incompatible evidence-backed
  donors **conflict** and de-adopt each other; the rest stay eligible.
- Mid-month begins (day > 15) take effect the following month (day-15 probe);
  the written begin day is preserved.  **Boundary-crossing** begins are
  **refused**, not clamped.  Everything is clamped to the **QCU data hull**.
- `residual-ambiguous` adoption is checked against this vintage's basis over
  the adoption window (`--current_offsets`); it is refused if the ambiguity
  members' offsets diverge there.
- `--promote-pha-only` (default off) lets an exact pha-only CONUS station with
  adoptable exterior hints emit a `24HR`-hull + exteriors timeline.
- `--skip-existing` is incompatible with `--hints`; a run refuses to consume
  its own hints dir (donor hints only).

To iterate on consolidation without re-solving (hours), or to backfill hints,
use the consolidate-only CLI (reads stored solutions + hint dirs, re-emits
`.his`; idempotent, TOBMain-free):

```bash
uv run python src/python/tob_hints.py consolidate --base data \
    --hints data-oldest/intermediate/hints [--dry-run] [--promote-pha-only]
uv run python src/python/tob_hints.py derive --base data   # (re)write hints
```

#### Phase 2: vintage hints influencing the solve (on by default; `--no-vintage-hints` to disable)

By default, `--hints` does two things. It extends the timeline outside the QCF
hull, which is consolidation. It also lets donor evidence influence the solve
inside the hull. Inside the hull the influence has two parts: a preference for
documented codes when the cost is equal, and a retry at a hinted boundary. The
retry accepts the new reading only if it has fewer deviants.
`--no-vintage-hints` limits `--hints` to consolidation.

The driver solves each station twice, with and without the vintage hints, and
compares the two readings by rank. A better reading keeps its natural class. If
the ranks are equal, the hint only tipped the choice, and that is a policy
adoption. Such regimes get the class `residual-proven-hinted`. No later run can
adopt them again. Thus a tie-break from a donor cannot become independent proof
in this vintage. The emitted `.his` is always a real reading that you can
verify bit-exactly.

### Optional verification gate

After TOBMain has been run over the emitted histories, you can optionally
check that the pipeline matches the stored solutions bit-exactly:

```bash
uv run python src/python/verify_his.py --jobs 8
```

Defaults assume the standard layout (`data/intermediate/tob/tavg`,
`data/input/raw/tavg`, `data/output/qcf/tavg`, `data/intermediate/solutions`).
All derived state (solutions, basis cache) lives under `<base>/intermediate`
and transient scratch under `<base>/scratch`, so a run never writes outside its
base dir. This step is **not** part of `quickstart_tob.sh`; run it when you want
an end-to-end gate.

### Scoring the PHA output

`verify_his.py` gates the *reconstruction*; these two measure how closely a full
TOB + PHA run reproduces NOAA's published QCF.

- `pha_fit_score.py` — composite fit over the whole corpus, weighting
  changepoint-date alignment 0.4, adjustment level 0.3 and data removal 0.3.
  `--per-station <tsv>` also writes the per-station breakdown, which is what
  makes a corpus number diagnosable rather than merely reportable.

  ```bash
  uv run python src/python/pha_fit_score.py \
      --tob  data/intermediate/tob/tavg \
      --qcf  data/output/qcf/tavg \
      --adj  data/output/adj/tavg \
      --per-station data/per_station.tsv
  ```

  Score against the TOB the run itself produced. Reusing a TOB from an earlier
  run measures two configurations at once: it is both PHA's input and the
  baseline the removal and level terms are computed against.

- `pha_fit_map.py` — plots that per-station TSV on a Mercator projection, so a
  clustered residual is visible as a place rather than a number. Needs the
  optional plotting extra:

  ```bash
  uv run --extra plot python src/python/pha_fit_map.py \
      --per-station data/per_station.tsv \
      --inventory data/intermediate/station.inv \
      --out fit_world.png --region world       # or: conus, europe
  ```

## Prerequisites

- `uv` for the managed environment (`uv sync` / `uv run`); see
  https://docs.astral.sh/uv/
- `bin/TOBMain` built with `TRIG_BACKEND=llvm-exact` for NOAA-faithful TOB
  arithmetic during reconstruction and verification
- Optional type checking: install Basilisk separately
  (`brew install Nimblesite/tap/basilisk`; https://www.basilisk-python.dev/) —
  it is **not** a PyPI package (the PyPI name collides with an unrelated project)

## Data Sources (Official Downloads)

GHCN-M v4 (QCU/QCF) direct download index:
```
https://www.ncei.noaa.gov/pub/data/ghcn/v4/
```

Typical filenames from that index:
```
ghcnm.tavg.latest.qcu.tar.gz
ghcnm.tavg.latest.qcf.tar.gz
```

HOMR PHR (Publication History Report) downloads:
```
https://www.ncei.noaa.gov/access/homr/reports
```

From the PHR section on that page:
```
https://www.ncei.noaa.gov/access/homr/file/phr.txt.zip
https://www.ncei.noaa.gov/access/homr/file/PHR_Table.txt
```

Enhanced MSHR downloads and layout:
```
https://www.ncei.noaa.gov/access/homr/file/mshr_enhanced.txt.zip
https://www.ncei.noaa.gov/access/homr/file/MSHR_Enhanced_Table.txt
```

## Notes

- All parsing is **fixed-width**, per the official dataset layouts.
- Output values from `compare_dirs.py` are in **Celsius** (hundredths scaled to degrees).
- Unit tests live under `src/python/tests/`; run with
  `uv run python -m unittest discover -s src/python/tests -v`.
