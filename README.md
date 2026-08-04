# GHCNM v4 (Reconstructed)

On March 20, 2025, NOAA published the GHCNM v4 source tarball:
`ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/source_code/ghcnm.src.v4.03172025.tar`

This repository is a buildable and testable version of that release. It adds
tooling and documentation.

## What this adds

NOAA's tarball does not contain these:

- Build control (`Makefile`, `generate_deps.py`)
- The test fixtures and configuration that the tests need
- The TOB pipeline (`TOBMain`, `TOBUtils`, `TOBTestUnits`)
- Station-history recovery and other Python helpers (`src/python`)
- The Go viewer webapp (`src/go`, built as `bin/PHAview`)
- This documentation

`docs/CHANGES.md` lists the changes to NOAA's own sources.

## Prerequisites

- `make`
- `gfortran`, or a compatible Fortran compiler
- `python3` and `uv`
- `gawk`
- `go` 1.22 or later, for `bin/PHAview`

## Build

```bash
make
```

This builds the Fortran programs, the AWK wrappers and `bin/PHAview`. Other
targets: `all`, `test`, `unit-test`, `output-test`, `phaview`, `clean`, `help`.

Build `bin/TOBMain` with `TRIG_BACKEND=llvm-exact` before you recover station
histories:

```bash
make TRIG_BACKEND=llvm-exact bin/TOBMain
```

This backend makes the float32 trigonometry give the same result on each
compiler. The recovery needs that.

## Quick start

```bash
./quickstart_tob.sh
```

The script downloads the published inputs, but only when the remote file is
newer. It then builds the workspace, recovers the histories, runs TOB and PHA,
and starts the viewer at `http://localhost:8080/`. Use `--no-viewer` to stop
the viewer.

To use the FTP endpoints for the GHCN files:

```bash
NOAA_GHCN_BASE_URL=ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4 ./quickstart_tob.sh
```

`docs/WORKFLOWS.md` gives the same sequence as separate commands.

## Documentation

- `docs/WORKFLOWS.md`: how to run the pipeline, the tests and Docker
- `docs/CHANGES.md`: what this reconstruction changes in NOAA's sources
- `docs/PROGRAMS.md`: what each program does
- `docs/DATA_FORMATS.md`: file formats and the recovered PHA configuration
- `src/python/README.md`: the Python helpers, and station-history recovery
- `src/go/README.md`: the viewer webapp

## Project layout

- `src/f`: Fortran 77 sources
- `src/f95`: Fortran 95 sources
- `src/incl`: Fortran include files
- `src/awk`: AWK scripts
- `src/go`: Go viewer webapp
- `src/python`: Python helpers
- `src/test/resources/data`: test fixtures
- `build/`: test and runtime properties, and generated logs
- `obj/`: object and module files (Git ignores these)
- `bin/`: compiled programs (Git ignores these)
