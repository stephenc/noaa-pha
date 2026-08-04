# Python Helpers (Project Additions)

These Python scripts are **project-specific helpers** added in this repository.
They are **not** part of the original NOAA source-code tarball. Their purpose is to:

- Prepare input data for the PHA pipeline from the published **QCU** dataset.
- Prepare comparable output data from the published **QCF** dataset.
- Reconstruct unpublished station history (`.his`) files bit-exactly from
  QCU/QCF residuals (CONUS) and HOMR metadata (non-CONUS).
- Optionally verify that a TOBMain run over those histories matches the
  solver solutions.
- Compare per-station outputs between two directories.

## What Each Script Does

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

### History reconstruction (primary feature)

**One command recovers the histories.** Given a prepared workspace it goes from
QCU + QCF + HOMR metadata (+ optional prior-vintage hints) to `.his` files:

`--hints` takes one directory and is repeatable, so a databank is passed as one
flag per vintage:

```bash
uv run python src/python/fetch_homr.py --base data        # metadata, once
args=(); for d in /path/to/hintstore/*/; do args+=(--hints "$d"); done
uv run python src/python/reconstruct_his.py --base data "${args[@]}" --jobs 16
```

Given a hint databank and the HOMR seed metadata, that one invocation is the
whole recovery. Four phases run in-process, so there is a single entry point and
no orchestration for a caller to get wrong:

1. **solve** — exact QCU/QCF residual decomposition into observation-time
   regimes, consuming donor hints from prior vintages (the slow part). Emission
   is restricted here to stations NOAA is responsible for
   (`--no-us-responsible`).
2. **PHR fill** — documents months no vintage constrains, under strict
   precedence *current-vintage solve > donor hints > PHR*
   (`--no-phr-fill`, `--phr-fill-regions pre,interior,post`).
3. **metadata rows** — rebuilds the non-TOB `.his` fields from HOMR
   (`--no-metadata-rows` for a strictly TOB-only history).
4. **COOP histories** — metadata-derived rows for CONUS COOP stations with no
   TOB solve (`--no-coop-history`). Runs last by necessity: it writes
   HOMR-derived files, which phases 2 and 3 must not then rewrite.

"Whole recovery" describes the pipeline, not the answer. The residual evidence
reaches exactly as far as QCF does — a QCU month with no QCF value has no
residual to decompose, so this vintage cannot resolve its observation time.
Vintages differ in which segments PHA retains and removes, so a month
unreachable here may carry a QCF value in another, and donor hints import what
those vintages could prove. Everything still unproven is documented from PHR and
HOMR. More vintages widen the proven fraction; none of them close it.

Phases 3 and 4 are idempotent and read only what the solve wrote, so they can be
re-run alone against a finished base to retune policy without repeating the
solve.

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

- `phr_fill.py` — fills months the residual solve leaves unconstrained with
  PHR-documented observation times. Precedence is enforced as a per-month
  authority mask: months inside a constrained run of this vintage's evidence,
  or covered by an adopted donor hint, are never touched. That mask is also
  what preserves the bit-exact QCF identity — a re-coded month could otherwise
  break `qcf == pha_qcf(t_out, S)`. Runs touching a QCF-present month, or an
  original (solved) regime begin, are refused whole and counted.

### Everything except observation time: `his_metadata.py`

`his_emit.emit_station_his` deliberately writes constant coordinates,
elevation, blank dist/dir and blank instruments — the "verbatim-field
invariant", which guarantees a TOB-only pipeline injects no phantom PHA
changepoint. The consequence is that PHA run with `pha.use-history-files = 1`
over such a history finds **no documented changepoints at all**:
`ReadInputFiles.f95` sets `history_code` from instrument height, instruments,
dist/dir or a position change — never from an observation-time change.

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

The TOB series is unaffected — TOBMain reads the observation time and (with
`tob.use-his-lat-lon=false`) takes coordinates from `station.inv`, ignoring
elevation, instruments and dist/dir. Verify rather than assume: rebuild the TOB
into a scratch directory from a `--no-metadata-rows` history and diff the two.

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

Every reconstruction run writes a per-station **hints** file to
`<base>/intermediate/hints/<sid>.hints.json` recording *what the residual
solve actually established* — constraint runs, per-regime coverage,
offset-ambiguity sets, and boundary evidence (schema `tob-hints/1`).  These
are a pure function of the residual solve (`--no-hints-out` suppresses them).

When another QCF vintage covers years this base does not, consolidate its
hints into this base's timeline — extending it **outside this vintage's QCF
hull only** (the solve is never hint-influenced in v1):

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

By default, `--hints` both extends the timeline *outside* the QCF hull
(consolidation) **and** lets donor evidence tip the **interior** solve — an
enumeration preference toward documented codes at equal cost, plus a
hinted-boundary retry that only accepts a *strictly* better (fewer-deviant)
reading. Pass `--no-vintage-hints` to restrict `--hints` to consolidation only
(the interior solve is then never hint-influenced).

To keep the interior influence honest, the station is solved twice (with and
without vintage hints) and compared by rank: a strict improvement keeps its
natural class, but an equal-rank reading the hint merely *tipped* is a policy
adoption — those regimes export as class `residual-proven-hinted` and are
**never** re-adoptable downstream, so a donor's tie-break can never launder into
this vintage's independent proof. The emitted `.his` is always a real,
bit-exactly verifiable reading. Run the cross-vintage experiment both ways
(`--no-vintage-hints` vs default) to measure it.

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
