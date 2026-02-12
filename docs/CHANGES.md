# Reconstruction Changes

The following changes have been made relative to NOAA's 2025-03-20 source tarball release (`ghcnm.src.v4.03172025.tar`):

- Added a `Makefile` and `generate_deps.py` to allow building the provided software.
- Added a `Dockerfile` and `.dockerignore` for reproducible cross-platform builds.
- Added `build/ghcnm-pha.test.properties` and `build/ghcnm-pha.unit-test.properties` to enable tests.
- Added test fixture files in `build/` and `src/test/resources/data/` required by reconstructed tests.
- Restored missing code paths/functions needed for compilation in selected Fortran sources.
- Added TOB adjustment pipeline (`TOBMain`, `TOBUtils`, `TOBTestUnits`) and supporting `tob.*` property keys.
- Added a Go-based viewer webapp (`src/go`, built as `bin/PHAview`) for interactive time-series and reference comparisons.
- Added Python helpers in `src/python/` for input preparation, history reconstruction, and output comparisons.
- Added repository-level documentation for build, workflows, programs, and formats.

The Go viewer webapp and Python helper scripts were added in this reconstruction and were **not** part of NOAA's original tarball.
