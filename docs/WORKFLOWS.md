# Workflows

## Running the PHA End-to-End

This section shows how to:
1) Fetch published QCU/QCF inputs plus PHR/MSHR metadata
2) Generate the `./data` workspace used by the pipeline
3) Run either the raw (no TOB) pipeline or the TOB+PHA pipeline
4) Compare outputs with published QCF

### 1) Download the inputs

```bash
mkdir -p ./data
# QCU / QCF
curl -o ./data/ghcnm.tavg.latest.qcu.tar.gz https://www.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tavg.latest.qcu.tar.gz
curl -o ./data/ghcnm.tavg.latest.qcf.tar.gz https://www.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tavg.latest.qcf.tar.gz

# PHR / MSHR (station histories)
curl -o ./data/phr.txt.zip https://www.ncei.noaa.gov/access/homr/file/phr.txt.zip
curl -o ./data/mshr_enhanced.txt.zip https://www.ncei.noaa.gov/access/homr/file/mshr_enhanced.txt.zip
```

**Note:** The `latest` QCU/QCF files are rolling and can change daily. NOAA
publishes an official archive of dated snapshots (since 2026-04-29) at
<https://www.ncei.noaa.gov/data/global-historical-climatology-network-monthly/v4/temperature/archive/>,
so a specific vintage can be fetched from there; keeping local copies of the
exact files you processed is still good practice.

### 2) Populate `./data` workspace

```bash
# Create input layout + properties
python3 src/python/qcu_to_inputs.py \
  --qcu-tar ./data/ghcnm.tavg.latest.qcu.tar.gz \
  --base data

# Extract build QCF outputs for comparison
python3 src/python/qcf_to_outputs.py \
  --qcf-tar ./data/ghcnm.tavg.latest.qcf.tar.gz \
  --base data
```

The station history files (`data/intermediate/history/*.his`) are not published
by NOAA.  Reconstruct them with `uv run python src/python/reconstruct_his.py`
(after the input/output layout is built and `bin/TOBMain` is compiled with
`TRIG_BACKEND=llvm-exact`): CONUS histories are solver-derived bit-exact
from the QCU/QCF residuals where the solver reports `exact=True`; non-CONUS
histories are metadata-derived from the MSHR/PHR records. The same command
provisions `data/intermediate/` (`history/`, filtered `station.inv`, and
`tob/tavg` out-dir) required by `tob.properties`.

Optional after a TOBMain run: verify that TOB output matches the stored
solutions bit-exactly (not required for normal use):

```bash
uv run python src/python/verify_his.py --jobs 8
```

See `src/python/README.md` for the full reconstruction CLI surface.

### 3) Run the pipeline

Raw (no TOB):
```bash
bin/PHAMain -p data/raw.properties
```

TOB + PHA:
```bash
bin/TOBMain -p data/tob.properties
bin/PHAMain -p data/tob.properties
```

### 4) Compare outputs

Both the raw and TOB pipelines write to the same output directory, so the same
comparison commands apply regardless of which path you ran.

Compare output vs QCU input (sanity check):
```bash
python3 src/python/compare_dirs.py \
  data/input/raw/tavg \
  data/output/adj/tavg \
  --header
```

Compare output vs published QCF:
```bash
python3 src/python/compare_dirs.py \
  data/output/adj/tavg \
  data/output/qcf/tavg \
  --header
```

Visualise changes

```bash
bin/PHAview \
  --inventory data/intermediate/station.inv \
  --history data/intermediate/history \
  --dir data/output/adj/tavg \
  --ref data/output/qcf/tavg \
  --ref2 data/intermediate/tob/tavg
```

## Running Tests

The project includes tests managed by the Makefile. Ensure any necessary input data is available (typically expected in a `data/` directory as configured in the `.properties` files).

*   **Run all tests:**
    ```bash
    make test
    ```
*   **Run only unit tests:**
    ```bash
    make unit-test
    ```
    *(Note: This currently runs the `PHATestUnits` executable with specific arguments defined in the Makefile. Test configuration is loaded from `build/ghcnm-pha.unit-test.properties`)*
*   **Run only output tests:**
    ```bash
    make output-test
    ```
    *(Note: This currently runs the `PHATestOutput` executable. Configuration details might be in `build/ghcnm-pha.test.properties` or similar)*

Test logs may be generated in the `build/` directory (e.g., `pha-unit-test.log`).

## Docker Usage

A `Dockerfile` is provided for building and running the project within a Docker container, simplifying dependency management.

1.  **Build the Docker Image:**
    Navigate to the project's root directory (containing the `Dockerfile`) and run:
    ```bash
    docker build -t noaa-ghcnm-v4 .
    ```
    *(You can replace `noaa-ghcnm-v4` with your preferred image name/tag)*

2.  **Run Commands Inside the Container:**
    To get an interactive shell inside the container:
    ```bash
    docker run -it --rm noaa-ghcnm-v4 bash
    ```
    *   The project code is located in `/app`.
    *   Compiled executables are in `/app/bin` and added to the container's `PATH`.
    *   You will probably want to use volume mounts to expose directories for input and output.
    *   You can execute programs directly from within the container.

    **Note:** The `Dockerfile` uses a multi-stage build. The final image is optimized for runtime and contains only the compiled executables and necessary runtime dependencies (like `gawk`). The intermediate `builder` stage contains the full build environment (`gfortran`, `make`, `python3`, source code) if needed for debugging the build process itself.

## Project Structure Notes

*   `src/f`: Contains Fortran 77 source files (`.f`).
*   `src/f95`: Contains Fortran 95 source files (`.f95`).
*   `src/incl`: Contains Fortran `INCLUDE` files (`.inc`).
*   `src/awk`: Contains AWK scripts (`.awk`) that are made executable in the build process.
*   `src/go`: Contains the added Go viewer webapp (UI + HTTP API). This directory was added in this reconstruction and was not included in the original NOAA source tarball.
*   `src/python`: Contains added Python helper scripts for input preparation, history reconstruction, and output comparison. This directory was added in this reconstruction and was not included in the original NOAA source tarball.
*   `src/test/resources/data`: Contains data files used for testing.
*   `build/`: Contains test configuration files (`.properties`) and potentially test logs.
*   `obj/`: Stores intermediate object (`.o`) and module (`.mod`) files (Git ignored).
*   `bin/`: Stores final compiled executables and executable scripts (Git ignored).
*   `deps.mk`: Auto-generated dependency file for Make (Git ignored).

## Viewer Webapp (Added)

An interactive viewer webapp has been added under `src/go` and is built to `bin/PHAview`.
It supports visualizing station series, QC include/ignore toggles, and comparisons against one or two references.

This webapp was not present in NOAA's original `ghcnm.src.v4.03172025.tar` tarball; it was added as part of this reconstructed repository.

For usage details, see `src/go/README.md`.

## Python Helpers (Added)

Python helper scripts have been added under `src/python` for:

- QCU/QCF workspace preparation (`qcu_to_inputs.py`, `qcf_to_outputs.py`)
- Bit-exact station-history reconstruction (`reconstruct_his.py` and supporting modules)
- Optional post-TOBMain verification (`verify_his.py`)
- Directory comparison (`compare_dirs.py`)

These helper scripts were not present in NOAA's original `ghcnm.src.v4.03172025.tar` tarball; they were added as part of this reconstructed repository. Details: `src/python/README.md`.

