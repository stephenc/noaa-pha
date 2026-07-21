# Reconstruction Changes

The following changes have been made relative to NOAA's 2025-03-20 source tarball release (`ghcnm.src.v4.03172025.tar`):

- Added a `Makefile` and `generate_deps.py` to allow building the provided software.
- Added a `Dockerfile` and `.dockerignore` for reproducible cross-platform builds.
- Added `build/ghcnm-pha.test.properties` and `build/ghcnm-pha.unit-test.properties` to enable tests.
- Added test fixture files in `build/` and `src/test/resources/data/` required by reconstructed tests.
- Restored missing code paths/functions needed for compilation in selected Fortran sources.
- Added fail-loud validation of `pha.snht-threshold` in NOAA's original `FindChangepoints.f95`. The original `get_critical_value()` handles only the three documented values (1 = 97.5%, 5 = 95%, 10 = 90%) with no `else`/default branch, so any other value silently left its critical-value table uninitialised — undefined behaviour that produced meaningless changepoints or crashed. The guard aborts with a clear message on unsupported values. **Behaviour is unchanged for every valid input**; only invalid input (which NOAA's own runs never pass) is affected.
- Added TOB adjustment pipeline (`TOBMain`, `TOBUtils`, `TOBTestUnits`) and supporting `tob.*` property keys.
- Added a Go-based viewer webapp (`src/go`, built as `bin/PHAview`) for interactive time-series and reference comparisons.
- Added Python helpers in `src/python/` for input preparation, history reconstruction, and output comparisons.
- Added repository-level documentation for build, workflows, programs, and formats.

The Go viewer webapp and Python helper scripts were added in this reconstruction and were **not** part of NOAA's original tarball.
