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

- `reconstruct_his.py` — driver CLI (default: full inventory, emission on).
  - Classifies stations, solves CONUS residuals, emits `.his` into
    `<base>/intermediate/history`, and provisions `<base>/intermediate/`
    (`station.inv` filtered to QCF stations + empty `tob/tavg` out-dir).
  - Example: `uv run python src/python/reconstruct_his.py --base data`

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

### Optional verification gate

After TOBMain has been run over the emitted histories, you can optionally
check that the pipeline matches the stored solutions bit-exactly:

```bash
uv run python src/python/verify_his.py --jobs 8
```

Defaults assume the standard layout (`data/intermediate/tob/tavg`,
`data/input/raw/tavg`, `data/output/qcf/tavg`, `work/solutions`). This step is
**not** part of `quickstart_tob.sh`; run it when you want an end-to-end gate.

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
