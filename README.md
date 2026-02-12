# GHCNM v4 (Reconstructed)

On March 20, 2025, NOAA published the GHCNM v4 source tarball:
`ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/source_code/ghcnm.src.v4.03172025.tar`

This repository is a reconstructed, buildable, and testable version of that release, with additional tooling and documentation.

## What Was Added

The following were added in this reconstruction and were not part of NOAA's original tarball:

- Build orchestration (`Makefile`, `generate_deps.py`)
- Additional test fixtures/configuration required to run reconstructed tests
- TOB pipeline reconstruction (`TOBMain`, `TOBUtils`, `TOBTestUnits`)
- Go viewer webapp (`src/go`, built as `bin/PHAview`)
- Python helper scripts (`src/python`) for data prep and comparison workflows
- Expanded project documentation

For the detailed change list, see `docs/CHANGES.md`.

## Prerequisites

- `make`
- `gfortran` (or compatible Fortran compiler)
- `python3`
- `gawk`
- `go` 1.22+ (for `bin/PHAview`)

## Build

```bash
make
```

This builds Fortran programs, AWK wrappers, and the Go viewer binary (`bin/PHAview`).

Useful targets:

- `make all`
- `make test`
- `make unit-test`
- `make output-test`
- `make phaview`
- `make clean`
- `make help`

## Quick Start (TOB + PHA)

1. Download published inputs and station-history sources:

```bash
mkdir -p data
curl -L -o data/ghcnm.tavg.latest.qcu.tar.gz https://www.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tavg.latest.qcu.tar.gz
curl -L -o data/ghcnm.tavg.latest.qcf.tar.gz https://www.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tavg.latest.qcf.tar.gz
curl -L -o data/phr.txt.zip https://www.ncei.noaa.gov/access/homr/file/phr.txt.zip
curl -L -o data/mshr_enhanced.txt.zip https://www.ncei.noaa.gov/access/homr/file/mshr_enhanced.txt.zip
```

2. Reconstruct the local input/output workspace:

```bash
python3 src/python/qcu_to_inputs.py --qcu-tar data/ghcnm.tavg.latest.qcu.tar.gz --base data
python3 src/python/qcf_to_outputs.py --qcf-tar data/ghcnm.tavg.latest.qcf.tar.gz --base data
```

3. Reconstruct TOB history files (`data/input/history/*.his`) from QCU/QCF deltas:

```bash
python3 src/python/qcufdelta_to_his.py \
  --inv data/input/station.inv \
  --qcu-dir data/input/raw/tavg \
  --qcf-dir data/output/qcf/tavg \
  --out-history-dir data/input/history \
  --mshr-zip data/mshr_enhanced.txt.zip \
  --tob-bin bin/TOBMain
```

4. Generate TOB-adjusted monthly data, then run PHA:

```bash
bin/TOBMain -p data/tob.properties
bin/PHAMain -p data/tob.properties
```

5. Optional: launch the viewer:

```bash
bin/PHAview --dir /path/to/primary --ref /path/to/reference --inventory /path/to/inventory
```

Then open: `http://localhost:8080/`

For alternative history reconstruction from PHR/MSHR, see `docs/WORKFLOWS.md`.
For viewer usage details, see `src/go/README.md`.

## Documentation

Detailed material has been split into `docs/`:

- `docs/README.md` (documentation index)
- `docs/CHANGES.md` (reconstruction changes)
- `docs/WORKFLOWS.md` (end-to-end workflows, tests, Docker)
- `docs/PROGRAMS.md` (program reference)
- `docs/DATA_FORMATS.md` (format reference)

## Project Layout

- `src/f`: Fortran 77 sources
- `src/f95`: Fortran 95 sources
- `src/incl`: Fortran include files
- `src/awk`: AWK scripts
- `src/go`: Go viewer webapp
- `src/python`: Python helper scripts
- `src/test/resources/data`: test data fixtures
- `build/`: test/runtime properties and generated logs
- `obj/`: object/module build artifacts
- `bin/`: compiled binaries/wrappers
