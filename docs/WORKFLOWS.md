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

**Note:** The `latest` QCU/QCF files are rolling and can change daily. NOAA does
not publish an official archive of daily snapshots, so reproducibility requires
you to keep your own copies.

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

There are two strategies for recovering the history files.

1. Reverse-engineer from the QCF-QCU delta (*recommended*)
    ```bash 
    python3 src/python/qcufdelta_to_his.py \
      --inv data/input/station.inv \
      --qcu-dir data/input/raw/tavg \
      --qcf-dir data/output/qcf/tavg \
      --mshr-zip mshr_enhanced.txt.zip \
      --out-history-dir data/input/history
   ```
2. Construct from the PHR records
    ```bash
    python3 src/python/phr_to_his.py \
      --inventory data/input/station.inv \
      --phr-zip phr.txt.zip \
      --mshr-zip mshr_enhanced.txt.zip \
      --out-dir data/input/history    
    ```

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
  --inventory data/input/station.inv \
  --dir data/output/adj/tavg \
  --ref data/input/raw/tavg \
  --ref2 data/output/qcf/tavg 
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

Python helper scripts have been added under `src/python` for data conversion and comparison workflows (for example, QCU/QCF transformations and directory comparisons).

These helper scripts were not present in NOAA's original `ghcnm.src.v4.03172025.tar` tarball; they were added as part of this reconstructed repository.

