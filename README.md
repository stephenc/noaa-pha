# GHCNM v4 (Reconstructed)

On the 20th of March 2025, NOAA made the [source code](ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/source_code/ghcnm.src.v4.03172025.tar) for the GHCNM v4 available.
Previously the only the `v52i` [source code](ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v3/software/52i/phav52i.tar.gz) for GHCNM v3 was available.

This new source code release does not include any instructions or even any way to compile.

## Changes from NOAA release on 2025-03-20

The following changes have been made in order to get the source code to compile and the unit tests to pass:

* Added a `Makefile` and a `generate_deps.py` to allow building the provided software
* Added a `Dockerfile` and `.dockerignore` to allow for a more reproducible build cross platform.
* Added a `build/ghcnm-pha.test.properties` and `build/ghcnm-pha.unit-test.properties` files to get the tests to pass (values were inferred as the ones necessary for the tests to all pass)
* Added a 7280 line file `build/ghcnm_20150121.inv` to get the tests to pass (contents were taken from an QCU inventory file, but seem unimportant other than the line count)
* Added an emp ty file `build/empty_file.txt` to get the tests to pass 
* Added a `build/test-station-meta-v3.txt` file with contents reconstructed from unit tests
* Restored the `hsort` function from the `v51i` version of the `src/f/filnet_subs.v4p.f` file in order to get the file to compile
* Restored the `gcd` function from the `v52i` version of the `src/f/ushcn_dist.v6.combo.f` file in order to get the file to compile
* Multiple changes in `src/f95/merge_d2m_mv2.f95` in order to get the file to compile (*NOTE* These fixes are perhaps the most speculative)
* Fixed Fortran 2018 deprecation warnings in `src/f95/stratus.f95`
* Added `src/main/.gitignore` to ensure the tests pass (a test checks for a "known good" `src/main` directory that was not included in the source extract)
* Added empty files: `src/test/resources/data/empty_file.txt`, `src/test/resources/data/ghcnm_20150121.dat`, `src/test/resources/data/ghcnm_20150121.inv`, `src/test/resources/data/test-station-corr.txt`, and `src/test/resources/data/test-station-meta-v3.txt` to ensure the tests pass (a test checks if a directory listing contains a specific set of filenames)
* Added a 7280 line file `src/test/resources/data/ghcnm_20150121.inv` to get the tests to pass (a copy of `build/ghcnm_20150121.inv`)
* Added `src/test/resources/data/test-station-meta-v3.txt` as a copy of `build/test-station-meta-v3.txt` to get the tests to pass
* Added a `.gitignore`
* Added this `README.md` file

## Prerequisites

Building this project requires the following tools installed on your system:

*   `make`
*   `gfortran` (or a compatible Fortran compiler, adjust `FC` in Makefile if needed)
*   `python3` (for dependency generation)
*   `gawk` (runtime dependency for some scripts)

Alternatively, you can use Docker to build and run the project in a containerized environment (see Docker Usage section below).

## Building the Project

This project uses `make` to manage the build process.

1.  **Generate Dependencies (Automatic):** The Makefile automatically runs `generate_deps.py` to scan Fortran source files and create `deps.mk` with the necessary dependencies.
2.  **Compile:** To compile all programs, simply run:
    ```bash
    make
    ```
    or explicitly:
    ```bash
    make all
    ```
    This will compile all Fortran 77 and Fortran 95 source files located in `src/f` and `src/f95` respectively.
3.  **Output:**
    *   Compiled object files (`*.o`, `*.mod`) will be placed in the `./obj/` directory.
    *   Final executables and executable scripts will be placed in the `./bin/` directory.
4.  **Clean:** To remove all generated build artifacts (object files, executables, `deps.mk`), run:
    ```bash
    make clean
    ```
5.  **Help:** For a list of all available make targets and their descriptions, run:
    ```bash
    make help
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
*   `src/test/resources/data`: Contains data files used for testing.
*   `build/`: Contains test configuration files (`.properties`) and potentially test logs.
*   `obj/`: Stores intermediate object (`.o`) and module (`.mod`) files (Git ignored).
*   `bin/`: Stores final compiled executables and executable scripts (Git ignored).
*   `deps.mk`: Auto-generated dependency file for Make (Git ignored).

## Inferred details

### Entrypoints

The following is in the inferred high-level functionality of the software: 

* PHAMain: Main driver program for the Pairwise Homogeneity Adjustment.
* PHATestOutput: Program to compare PHA output against expected results.
* PHATestUnits: Program to run unit tests for various modules.
* climat_decoder: Decodes CLIMAT messages.
* gen-mon-composites.v2: Generates monthly composite data (likely an input preparation step).
* ghcnm_clean: Cleans GHCN-M data based on record length and expert assessment lists.
* ghcnm_climat: Replaces flagged data with CLIMAT source data.
* ghcnm_edit: Edits specific data points or metadata based on input files.
* ghcnm_qc: Performs Quality Control checks (duplicate years, record extremes, streaks, spatial checks).
* merge_d2m_mv2: Merges GHCN-Daily-to-Monthly and GHCNMv2 data (likely input prep).
* merge_update: Updates merged data with newer sources.
* stage3_to_ghcnm: Converts Stage 3 merged data to GHCN-M format.
