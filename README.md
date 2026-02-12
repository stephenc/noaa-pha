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
* Added TOB adjustment pipeline (`TOBMain`, `TOBUtils`, `TOBTestUnits`) and supporting TOB property keys (`tob.*`).  
  The TOB pipeline was reconstructed by combining the documented fragments in the
  NOAA v4 source with the fully working v52i implementation. Where v4 logic was
  incomplete, the v52i behavior was used to fill in gaps while preserving the
  original v4 interfaces and data flow.

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

# Programs

## PHAMain

This program appears to be the main driver for the Pairwise Homogeneity Adjustment (PHA) process. It likely orchestrates the various steps involved in identifying and adjusting inhomogeneities in climate data series. Based on the properties files, it reads station metadata, element data, neighbor information (distance and correlation), and station history files. It then applies the PHA algorithm, potentially involving steps like changepoint detection, attribution, and adjustment calculation, writing the adjusted data to an output directory. The specific behavior is heavily configured through a `.properties` file.

### Usage

This program does not seem to take direct command-line arguments for its core functionality. Instead, it reads configuration parameters from a properties file (e.g., `ghcnm-pha.properties`). Key properties control input/output paths, the element being processed (tmax, tmin, tavg), version identifiers, and numerous algorithm tuning parameters for neighbor selection, changepoint detection, and adjustment methods.

### Example

```bash
# Ensure ghcnm-pha.properties is configured correctly
PHAMain
```
*(Note: Execution likely requires specific input data files and directories to be present as defined in the properties file).*

## TOBMain

*NOTE:* This program has been reconstructed based on the TOB code in v4 as well as the previous v3 TOB code.
The Fortran 95 code for TOB adjustment was not part of the original tarball from NOAA.

This program applies the Time of Observation Bias (TOB) adjustments to monthly data.
It reads station metadata and `.his` history, computes Karl et al. bias tables, and
writes adjusted data to the TOB output directory specified by `tob.*` properties.

### Usage

TOBMain is configured via a properties file (e.g., `tob.properties`) and uses
`tob.path.station-element-data-in`, `tob.path.station-element-data-out`, and
`tob.start-year` (optionally overridden by `tob.start-from-history`). It also
uses `tob.input-data-type` to determine the per-station input file suffix.
Additional flags include `tob.backfill-if-first-nonblank` and
`tob.pause-on-blank-after-nonblank`.

### Example

```bash
bin/TOBMain -p data/tob.properties
```

## PHATestOutput

This program is designed to test the output of the PHA process. It likely compares the adjusted data generated by `PHAMain` (or a similar process) against a set of expected or known "good" results. This is crucial for verifying the correctness and consistency of the homogenization algorithm's implementation. Configuration details, such as the paths to the actual and expected output files, are probably specified in a properties file (e.g., `ghcnm-pha.test.properties`).

### Usage

Similar to `PHAMain`, this program likely reads its configuration from a properties file (`ghcnm-pha.test.properties` is used in the Makefile). It doesn't appear to take direct command-line arguments for file paths but relies on the properties for locating the output to test and the expected results.

### Example

```bash
# Ensure ghcnm-pha.test.properties points to the correct output and expected files
PHATestOutput
```
*(Note: This requires prior PHA output and corresponding expected output files).*

## PHATestUnits

This program executes unit tests for various modules within the PHA codebase. Unit tests typically verify the functionality of individual components (subroutines or functions) in isolation. It ensures that specific parts of the code behave as expected under controlled conditions. The Makefile invokes this with specific arguments (`-d 20160316 -longflag test`), suggesting it might test date handling or specific flag functionalities, and it loads configuration from `ghcnm-pha.unit-test.properties`.

### Usage

The program is invoked with command-line arguments, as seen in the Makefile.

```
PHATestUnits [options]
```

*   `-d <date>`: Specifies a date (e.g., `20160316`).
*   `-longflag <value>`: Specifies a value for a long flag test (e.g., `test`).
*   Reads configuration from `ghcnm-pha.unit-test.properties`.

### Example

```bash
# As used in the Makefile's unit-test target
PHATestUnits -d 20160316 -longflag test
```

## average_network

This AWK script calculates the average value for a network of stations over a specified time period. It reads a list of station IDs from an input file, accesses the corresponding data files (assuming a specific directory structure and naming convention based on element, process type, and station ID), and computes the monthly and annual averages across all provided stations. The script requires several variables (`BYEAR`, `EYEAR`, `ELEM`, `PROC`, `DIV`, `DDIR`) to be set via the `-v` option.

### Usage

```bash
awk -f average_network.awk -v BYEAR=<start_year> -v EYEAR=<end_year> \
    -v ELEM=<element> -v PROC=<process_type> -v DIV=<divisor> \
    -v DDIR=<data_directory> <station_list_file>
```

*   `BYEAR`: The beginning year for the averaging period.
*   `EYEAR`: The ending year for the averaging period.
*   `ELEM`: The climate element (e.g., `tavg`, `tmax`).
*   `PROC`: The processing stage of the data (e.g., `raw`, `adj`).
*   `DIV`: The divisor to apply to the raw data values (e.g., `100` for hundredths of degrees).
*   `DDIR`: The base directory containing the processed data, organized by `PROC`.
*   `<station_list_file>`: A file containing the list of station IDs (one per line) to include in the network average.

### Example

```bash
awk -f average_network.awk -v BYEAR=1875 -v EYEAR=2011 -v ELEM=tavg \
    -v PROC=raw -v DIV=100 -v DDIR=~/data station_list.txt > network_average.txt
```

## average_stn

This AWK script calculates the average monthly values for individual stations over a specified time period. It reads a list of station IDs from an input file, processes each station one by one, reads its data file, and calculates the average for each month within the specified year range (`BYEAR` to `EYEAR`). The output is the station ID followed by the 12 monthly averages. Like `average_network`, it requires several variables (`BYEAR`, `EYEAR`, `ELEM`, `PROC`, `DIV`, `DDIR`) set via `-v`.

### Usage

```bash
awk -f average_stn.awk -v BYEAR=<start_year> -v EYEAR=<end_year> \
    -v ELEM=<element> -v PROC=<process_type> -v DIV=<divisor> \
    -v DDIR=<data_directory> <station_list_file>
```

*   `BYEAR`: The beginning year for the averaging period.
*   `EYEAR`: The ending year for the averaging period.
*   `ELEM`: The climate element (e.g., `tavg`, `tmax`).
*   `PROC`: The processing stage of the data (e.g., `raw`, `adj`).
*   `DIV`: The divisor to apply to the raw data values (e.g., `100`).
*   `DDIR`: The base directory containing the processed data, organized by `PROC`.
*   `<station_list_file>`: A file containing the list of station IDs (one per line) to process.

### Example

```bash
awk -f average_stn.awk -v BYEAR=1875 -v EYEAR=2011 -v ELEM=tavg \
    -v PROC=raw -v DIV=100 -v DDIR=~/data station_list.txt > station_monthly_averages.txt
```

## climat_decoder

This program decodes CLIMAT messages, which are meteorological reports summarizing monthly weather conditions for specific stations. It reads a file containing CLIMAT messages (likely in a specific format, potentially mixed with other text) and extracts monthly mean temperature (`TAVG`), maximum temperature (`TMAX`), minimum temperature (`TMIN`), and precipitation (`PRCP`) values. It outputs these decoded values associated with their WMO station identifier, year, and month.

### Usage

```bash
climat_decoder <input_climat_file> <output_decoded_file>
```

*   `<input_climat_file>`: Path to the file containing the CLIMAT messages to be decoded.
*   `<output_decoded_file>`: Path to the file where the decoded data will be written. The output format appears to be: `WMO_ID Year Month TMAX TAVG TMIN PRCP`.

### Example

```bash
climat_decoder raw_climat_messages.txt decoded_climat_data.txt
```

## compare.mm_avg

This AWK script compares monthly average temperature (`TAVG`) values with values calculated as `(TMAX + TMIN) / 2` within GHCN-M v2 data files. It reads GHCNMv2 formatted data, likely containing TMAX, TMIN, and TAVG records for multiple stations. For each station, it calculates the average from TMAX and TMIN for each month/year and compares it to the reported TAVG, printing records where the difference exceeds a small tolerance (accounting for rounding). It also prints a summary count of matching records per station.

### Usage

```bash
awk -f compare.mm_avg.awk <ghcnm_v2_data_file>
```

*   `<ghcnm_v2_data_file>`: Input file containing GHCNMv2 data sorted by station ID, including TMAX, TMIN, and TAVG records.

### Example

```bash
awk -f compare.mm_avg.awk v2.mean_cone.nodups > comparison_report.txt
```

## convert.pha2cas

This AWK script converts data processed by the PHA (Pairwise Homogeneity Adjustment) system into the Climate Assessment System (CAS) 3-flag GHCNM format. It takes GHCN and USHCN data from specified input directories (`GDIR`, `UDIR`) at a certain processing level (`PROC`), processes a specific element (`ELEM`), and uses a station inventory file (likely `ghcnm.inv`) to guide the process. It generates two output files: one with the formatted data (`OUTDAT`) and one with the inventory of stations included (`OUTINV`). The script handles flag conversions based on the processing stage (raw, WMs, FLs).

### Usage

```bash
awk -f convert.pha2cas.awk -v PROC=<proc_level> -v ELEM=<element> \
    -v GDIR=<ghcn_dir> -v UDIR=<ushcn_dir> -v OUTDAT=<output_data_file> \
    -v OUTINV=<output_inv_file> <input_inventory_file>
```

*   `PROC`: The processing level (e.g., `raw`, `WMs.52g`, `FLs.52g`).
*   `ELEM`: The climate element (e.g., `tavg`, `tmax`, `tmin`).
*   `GDIR`: Input directory for GHCN data.
*   `UDIR`: Input directory for USHCN data.
*   `OUTDAT`: Output data file path.
*   `OUTINV`: Output inventory file path.
*   `<input_inventory_file>`: The master inventory file (e.g., `ghcnm.inv`).

### Example

```bash
awk -f convert.pha2cas.awk -v PROC=raw -v ELEM=tavg -v GDIR=gdata \
    -v UDIR=udata -v OUTDAT=ghcnm.52d.dat -v OUTINV=ghcnm.52d.inv ghcnm.inv
```

## convert_mv2_d2m

This AWK script converts GHCNMv2 formatted data into the GHCN-Daily-to-Monthly (D2M) 3-flag format. It reads a GHCNMv2 data file, parses each record (station ID, year, element, monthly values/flags), and writes the data into separate files for each station and element within a specified output directory (`ODIR`) and processing subdirectory (`PROC`). The output files follow the naming convention `<station_id>.<proc>.<element>`.

### Usage

```bash
awk -f convert_mv2_d2m.awk -v PROC=<proc_level> -v ODIR=<output_directory> \
    <ghcnm_v2_input_file>
```

*   `PROC`: The processing level identifier to use in the output directory structure and filenames (e.g., `raw`, `tob`, `adj`).
*   `ODIR`: The base output directory where the `<PROC>` subdirectory will be created.
*   `<ghcnm_v2_input_file>`: The input data file in GHCNMv2 format.

### Example

```bash
awk -f convert_mv2_d2m.awk -v PROC=raw -v ODIR=../global v2.mean_cone.nodups
```

## count_edit_pha

This AWK script counts the occurrences of specific Quality Control (QC) flags ('X' and 'Q') within a PHA-processed data file (likely the adjusted or filled data). It reads data formatted in the GHCNM 3-flag monthly style, iterates through each month of each year, and tallies the counts for 'X' flags (indicating PHA removal) and 'Q' flags (indicating data filled due to QC flags in earlier stages). The output is a time series showing the counts of 'X', 'Q', and valid data values ('N') per month/year.

### Usage

```bash
awk -f count_edit_pha.awk <pha_output_data_file>
```

*   `<pha_output_data_file>`: An input data file processed by PHA, expected in the 11-character ID, 3-flag monthly format (e.g., `station_id.FLs.tmax`).

### Example

```bash
awk -f count_edit_pha.awk station_id.FLs.tmax > flag_counts_pha.txt
```

## count_edit_qc

This AWK script counts the occurrences of various Quality Control (QC) flags within a data file, likely one that has undergone initial QC checks (pre-PHA). It reads data formatted in the QCA (Quality Controlled Assessment?) style (5-digit value, 3 flags), focusing on recent years (>= 2014 in the script). It tallies the total count for each unique QC flag encountered (flag in position 2) and outputs a summary table followed by a time series showing the counts per flag type for each month/year.

### Usage

```bash
awk -f count_edit_qc.awk <qc_output_data_file>
```

*   `<qc_output_data_file>`: An input data file processed by an initial QC step, expected in the QCA format (e.g., `ghcnm.qca.dat`).

### Example

```bash
awk -f count_edit_qc.awk ghcnm.qca.dat > flag_counts_qc.txt
```

## count_obs

This AWK script counts the number of valid monthly observations for a given station within a specified year range (`BYEAR` to `EYEAR`). It reads a single station's data file (GHCNM 3-flag format), counts months where the data is not missing and the QC flag (position 2) is either blank or 'E' (Estimated/Filled). If the count for *every* month meets or exceeds a threshold (`THRES`), it prints the station ID followed by the counts for each of the 12 months.

### Usage

```bash
awk -f count_obs.awk -v BYEAR=<start_year> -v EYEAR=<end_year> \
    -v THRES=<min_monthly_obs> <station_data_file>
```

*   `BYEAR`: The beginning year for the counting period.
*   `EYEAR`: The ending year for the counting period.
*   `THRES`: The minimum number of observations required for *each* month within the period for the station record to be printed.
*   `<station_data_file>`: The input data file for a single station in GHCNM 3-flag format.

### Example

```bash
awk -f count_obs.awk -v BYEAR=1981 -v EYEAR=2010 -v THRES=8 station_id.raw.tmax
```
*(This example would print the station ID and monthly counts only if the station has at least 8 observations for every month between 1981 and 2010).*

## filter_a_with_b.ghcn

This AWK script filters records from an input file (`FILENAME`) based on whether the station ID (first 11 characters of the first field) exists in a separate station list file (`STNLIST`). It assumes both files are sorted by station ID. It produces three output files: `<FILENAME>.input_in_stnlist` (records from input file whose station ID is in `STNLIST`), `<FILENAME>.input_not_stnlist` (records from input file whose station ID is *not* in `STNLIST`), and `<FILENAME>.stnlist_not_input` (records from `STNLIST` whose station ID is *not* in the input file).

### Usage

```bash
awk -f filter_a_with_b.ghcn.awk -v STNLIST=<station_list_file> <input_data_file>
```

*   `STNLIST`: Path to the file containing the list of station IDs to filter against.
*   `<input_data_file>`: The main input file to be filtered. Both files must be sorted by the 11-character station ID in the first field.

### Example

```bash
awk -f filter_a_with_b.ghcn.awk -v STNLIST=NWS_subset.txt ghcnm.inv
```
*(This would create `ghcnm.inv.input_in_stnlist`, `ghcnm.inv.input_not_stnlist`, and `ghcnm.inv.stnlist_not_input` based on the IDs in `NWS_subset.txt`)*.

## gen-mon-composites.v2

This Fortran program generates monthly composite data series, likely by combining data from different sources or stations according to specific rules defined in external files (USHCN v2 station list and composite list). It reads metadata about the primary stations and their potential composite sources, then reads the corresponding monthly data (TMAX, TMIN, PRCP) from GHCN-Daily formatted files. It merges these data sources based on specified date ranges for each composite, potentially filling gaps in the primary station record. The output is a set of monthly data files for the composite stations.

### Usage

```bash
gen-mon-composites.v2 <base_dir> <hcn_station_list> <hcn_composite_list> <coop_region_dir>
```

*   `<base_dir>`: The base directory where input data (like `hcnv1/raw/`) and output data (`hcnv2/raw/`) are located/created.
*   `<hcn_station_list>`: Path to the USHCN v2 station list file (e.g., `ushcn-v2-stations.txt`).
*   `<hcn_composite_list>`: Path to the USHCN v2 composite definition file (e.g., `ushcn-v2-composites.txt`).
*   `<coop_region_dir>`: Identifier for the COOP network region subdirectory (e.g., `coop`) under `<base_dir>` where composite source data resides.

### Example

```bash
gen-mon-composites.v2 /data/ghcnm/ ushcn-v2-stations.txt ushcn-v2-composites.txt coop
```

## gen_avg.3flg

This AWK script calculates monthly average temperature (`tavg`) by averaging the corresponding monthly maximum (`tmax`) and minimum (`tmin`) temperature values. It processes a list of stations provided as input, reads the `tmax` and `tmin` data files for each station from a specified directory structure (`ddir`/`vproc`), calculates `(tmax + tmin + 1) / 2` (integer division implied by context, +1 likely for rounding), and writes the resulting `tavg` data to a new file in the same directory structure. It also handles flag propagation based on the input flags and processing level (`vproc`).

### Usage

```bash
awk -f gen_avg.3flg.awk -v byear=<start_year> -v eyear=<end_year> \
    -v ddir=<data_directory> -v vproc=<process_level> <station_list_file>
```

*   `byear`: The beginning year for processing.
*   `eyear`: The ending year for processing.
*   `ddir`: The base data directory containing the `<vproc>` subdirectories.
*   `vproc`: The processing level identifier (e.g., `FLs.52d`, `raw`) used for input/output paths and flag handling logic.
*   `<station_list_file>`: A file containing the list of station IDs to process.

### Example

```bash
awk -f gen_avg.3flg.awk -v byear=1895 -v eyear=2005 \
    -v ddir=~/CDMP_3220.2/monthly -v vproc=FLs.52d \
    station_list.txt > gen_avg.log
```

## gen_avg_dtr.3flg

This AWK script is similar to `gen_avg.3flg` but can generate either average temperature (`tavg`) or diurnal temperature range (`tdtr`, calculated as `tmax - tmin`), controlled by the `otype` variable. It processes a list of stations, reads `tmax` and `tmin` data, performs the selected calculation, and writes the result (`tavg` or `tdtr`) to an output file. Additionally, it checks if each station has a minimum number of observations (8) for *every* month within the specified period; if not, the station's metadata record is written to a separate file (`ometa`.rem) and the station's data is not generated. Otherwise, the metadata is written to the main output metadata file (`ometa`).

### Usage

```bash
awk -f gen_avg_dtr.3flg.awk -v byear=<start_year> -v eyear=<end_year> \
    -v ddir=<data_directory> -v vproc=<process_level> \
    -v otype=<AVG|DTR> -v ometa=<output_meta_file> <station_list_file_with_meta>
```

*   `byear`: The beginning year for processing.
*   `eyear`: The ending year for processing.
*   `ddir`: The base data directory.
*   `vproc`: The processing level identifier.
*   `otype`: Output type, either `AVG` or `DTR`.
*   `ometa`: The base path for the output metadata file. A `.rem` suffix will be added for stations failing the minimum observation check.
*   `<station_list_file_with_meta>`: A file containing station IDs *and* their metadata records (the script uses `$0` which implies the whole line is needed).

### Example

```bash
awk -f gen_avg_dtr.3flg.awk -v byear=1895 -v eyear=2005 \
    -v ddir=data -v vproc=FLs.52d -v otype=AVG \
    -v ometa=ghcnm.avg.meta station_metadata.inv > gen_avg_dtr.log
```

## gen_dtr.3flg

This AWK script calculates the monthly diurnal temperature range (`tdtr`) as `tmax - tmin`. It processes a list of stations, reads the corresponding `tmax` and `tmin` data files from a specified directory structure (`ddir`/`vproc`), performs the subtraction for each month where both values are available, and writes the resulting `tdtr` data to a new file. It propagates flags similarly to `gen_avg.3flg`. It prints a summary line to standard output for each station processed, indicating the number of records written.

### Usage

```bash
awk -f gen_dtr.3flg.awk -v byear=<start_year> -v eyear=<end_year> \
    -v ddir=<data_directory> -v vproc=<process_level> <station_list_file>
```

*   `byear`: The beginning year for processing.
*   `eyear`: The ending year for processing.
*   `ddir`: The base data directory.
*   `vproc`: The processing level identifier.
*   `<station_list_file>`: A file containing the list of station IDs to process.

### Example

```bash
awk -f gen_dtr.3flg.awk -v byear=1895 -v eyear=2011 -v ddir=hcnv2 \
    -v vproc=tob cdmp.meta.nws > gen_dtr.log
```

## gen_meta

This AWK script generates element-specific metadata files based on a master inventory file and the availability and quality of data in individual station data files. It reads a master inventory (like `ghcnm.inv`), then for each station, it checks the data files for `tmax`, `tmin`, `tavg`, and `prcp` within a specified directory (`IDIR`/`PROC`). It determines the period of record (POR) for each element and checks if the station meets a minimum monthly observation threshold (e.g., 6 or 8 months, depending on `REG`). If a station passes the threshold for an element, its metadata record is written to an element-specific output file (e.g., `<META>.tmax`). It also prints the total number of stations passing for each element and exports the total years of record for each element as environment variables.

### Usage

```bash
awk -f gen_meta.awk -v PROC=<proc_level> -v IDIR=<input_dir> \
    -v META=<output_meta_base> -v REG=<region> <master_inventory_file>
```

*   `PROC`: The processing level identifier (e.g., `raw`, `tob`).
*   `IDIR`: The base input directory containing the `<PROC>` subdirectories with station data files.
*   `META`: The base path for the output element-specific metadata files (e.g., `ghcn.meta`).
*   `REG`: The region identifier (e.g., `coop`, `global`, `northam`) which affects the minimum observation threshold.
*   `<master_inventory_file>`: The input master inventory file (e.g., `ghcnm.inv`).

### Example

```bash
awk -f gen_meta.awk -v PROC=raw -v IDIR=../global -v META=ghcn.meta \
    -v REG=global ghcnm.inv
```

## ghcnm_clean

This Fortran program cleans the GHCNM dataset based on data record length and expert assessment lists. It reads an input inventory (`<IN_METAFILE>`) and data file (`<IN_DATAFILE>`), checks each station against a minimum data threshold (number of months with data >= `YEAR_THRESHOLD`), and removes stations that fall below this threshold unless they are on an explicit keep list (`<IN_KEEP-FILE>`). It also removes stations listed in a separate removal file (`<IN_REM-FILE>`). Finally, it adds an asterisk (*) to the metadata of stations specified in an asterisk list (`<IN_ASTK-FILE>`), likely indicating stations suitable for PHA. The cleaned data and metadata are written to output files (`<OUT_DATAFILE>`, `<OUT_METAFILE>`).

### Usage

```bash
ghcnm_clean <IN_METAFILE> <IN_DATAFILE> <YEAR_THRESHOLD> <IN_REM-FILE> \
            <IN_ASTK-FILE> <IN_KEEP-FILE> <OUT_METAFILE> <OUT_DATAFILE>
```

*   `<IN_METAFILE>`: Input GHCNM inventory file.
*   `<IN_DATAFILE>`: Input GHCNM data file.
*   `<YEAR_THRESHOLD>`: Minimum number of years (implicitly, months checked) required for a station to be kept.
*   `<IN_REM-FILE>`: File listing station IDs to remove explicitly.
*   `<IN_ASTK-FILE>`: File listing station IDs to mark with an asterisk.
*   `<IN_KEEP-FILE>`: File listing station IDs to keep, overriding the year threshold.
*   `<OUT_METAFILE>`: Output cleaned inventory file.
*   `<OUT_DATAFILE>`: Output cleaned data file.

### Example

```bash
ghcnm_clean ghcnm.v4.0.0.inv ghcnm.v4.0.0.dat 10 \
            resources/stations_to_remove.txt \
            resources/stations_to_asterisk.txt \
            resources/stations_to_keep.txt \
            ghcnm.v4.0.1.inv ghcnm.v4.0.1.dat
```

## ghcnm_climat

This Fortran program updates a GHCNM dataset by replacing data values flagged with 'S' (source unknown or suspect) with corresponding values from a more reliable CLIMAT source, specifically data processed by the UK Met Office. It reads an input GHCNM inventory and data file, identifies stations present in a WMO cross-reference list, checks for corresponding Met Office CLIMAT data files, and replaces 'S'-flagged monthly temperature values (TMAX, TMIN, TAVG) with the CLIMAT values, changing the source flag to '|'. The updated data is written to a new output data file.

### Usage

```bash
ghcnm_climat <DATABANK_DIRECTORY> <IN_METAFILE> <IN_DATAFILE> <OUT_DATAFILE>
```

*   `<DATABANK_DIRECTORY>`: Base directory containing resources like the WMO cross-reference and the CLIMAT data (e.g., in `tmp/climat-uk/`).
*   `<IN_METAFILE>`: Input GHCNM inventory file.
*   `<IN_DATAFILE>`: Input GHCNM data file containing 'S' flagged data.
*   `<OUT_DATAFILE>`: Output data file with 'S' flags potentially replaced by '|' flags and CLIMAT data.

### Example

```bash
ghcnm_climat /path/to/databank ghcnm.v4.0.1.inv ghcnm.v4.0.1.dat ghcnm.v4.0.1.climat_updated.dat
```

## ghcnm_edit

This Fortran program edits a GHCNM dataset based on instructions provided in external edit files. It can modify individual data points (value and flags) for specific stations, elements, years, and months based on a data edit file (`<DATA_EDITFILE>`). It can also update station metadata (name, latitude, longitude, elevation) based on a metadata edit file (`<META_EDITFILE>`). It reads the input inventory and data, applies the specified edits, and writes the modified inventory and data to new output files.

### Usage

```bash
ghcnm_edit <IN_METAFILE> <IN_DATAFILE> <META_EDITFILE> <DATA_EDITFILE> <OUT_META> <OUT_DATA>
```

*   `<IN_METAFILE>`: Input GHCNM inventory file.
*   `<IN_DATAFILE>`: Input GHCNM data file.
*   `<META_EDITFILE>`: File containing metadata corrections/updates.
*   `<DATA_EDITFILE>`: File containing data point corrections/updates.
*   `<OUT_META>`: Output inventory file with edits applied.
*   `<OUT_DATA>`: Output data file with edits applied.

### Example

```bash
ghcnm_edit ghcnm.v4.0.1.inv ghcnm.v4.0.1.dat resources/ghcnmv4_edit_meta.txt \
           resources/edit.dat ghcnm.v4.0.2.inv ghcnm.v4.0.2.dat
```

## ghcnm_qc

This Fortran program performs various Quality Control (QC) checks on GHCNM data. It reads an input inventory and data file and applies checks including: identifying duplicate years within a station's record, checking for world record exceedances, detecting unrealistic streaks of identical values, performing spatial consistency checks against neighboring stations (comparing Z-scores), and checking for internal consistency (TMAX < TMIN). Data points failing these checks have their QC flag updated accordingly (e.g., 'D' for duplicate year, 'R' for record extreme, 'K' for streak, 'S' or 'T' for spatial outliers, 'I' for internal inconsistency). The program outputs the data file with updated QC flags and a diagnostics file detailing the flags applied.

### Usage

```bash
ghcnm_qc <METADATAFILE> <DATAFILE> <PARAMETER_FILE>
```

*   `<METADATAFILE>`: Input GHCNM inventory file.
*   `<DATAFILE>`: Input GHCNM data file.
*   `<PARAMETER_FILE>`: Configuration file containing thresholds and parameters for the QC checks (e.g., world extremes, Z-score thresholds, streak length).

### Example

```bash
ghcnm_qc ghcnm.v4.0.2.inv ghcnm.v4.0.2.dat resources/qc_params.dat
```
*(Outputs `ghcnm_qc.out` and `ghcnm_qc.diagnostics`)*.

## join_gv4

This AWK script joins GHCN data files from a specified directory (`GDIR`) and processing level (`PROC`) into a single large data file (`OUTDAT`) and corresponding inventory file (`OUTINV`), based on an input station list (either GHCNM QC inventory format or `gen_meta` log format, specified by `INTYPE`). It processes a specific element (`ELEM`) and converts flags based on the processing level and QAEF flag (Quality Assessed/Estimated Flag?). It handles potential differences in input formats and applies specific logic for flag conversion, particularly for 'WMs' (Williams-Menne adjusted) and 'FLs' (Filled) processing levels.

### Usage

```bash
awk -f join_gv4.awk -v PROC=<proc_level> -v ELEM=<element> -v GDIR=<ghcn_dir> \
    -v OUTDAT=<output_data_file> -v OUTINV=<output_inv_file> \
    -v QAEF=<qae_flag> -v INTYPE=<INV|META> <input_station_list>
```

*   `PROC`: The processing level (e.g., `raw`, `WMs.52g`, `FLs.52g`).
*   `ELEM`: The climate element (e.g., `tavg`, `tmax`, `tmin`).
*   `GDIR`: Input directory for GHCN data files.
*   `OUTDAT`: Output data file path (concatenated data).
*   `OUTINV`: Output inventory file path (for stations included).
*   `QAEF`: Quality Assessed/Estimated Flag type (e.g., `qcf`, `qaf`, `qfe`). Affects flag handling and potentially date ranges.
*   `INTYPE`: Format of the `<input_station_list>` (`INV` or `META`).
*   `<input_station_list>`: File containing the list of stations to process.

### Example

```bash
awk -f join_gv4.awk -v PROC=raw -v GDIR=gdata -v ELEM=tavg -v QAEF=qcf \
    -v INTYPE=INV -v OUTDAT=ghcnm.52d.dat -v OUTINV=ghcnm.52d.inv ghcnm.inv
```

## merge_d2m_mv2

This Fortran program merges GHCN-Daily-to-Monthly (D2M) data with GHCNMv2 data. It appears to read data from both sources (likely pre-processed into specific formats or locations), potentially prioritizing one source over the other or combining them based on data availability or quality flags. The specifics of the merging logic (e.g., handling overlaps, flag precedence) are contained within the Fortran code. The output is likely a merged dataset in a format suitable for further processing, possibly the GHCNM 3-flag format. *Note: The README mentions fixes in this file are speculative, indicating its complexity or lack of clear documentation.*

### Usage

The program likely requires specific input files or directories configured either internally or through environment variables, as direct command-line arguments are not apparent from the provided context.

### Example

*(No clear example can be derived from the provided context. Execution would depend on understanding the internal file path assumptions or required environment variables.)*

## merge_update

This Fortran program updates a merged GHCNM dataset with data from newer sources. It reads a base merged dataset (presumably the output of a previous merge step like `merge_d2m_mv2` or the main Stage 3 merge) and incorporates data from various update sources (like GHCN-D, different CLIMAT streams). The update logic likely involves appending recent data, potentially replacing older data based on source priority or flags, and ensuring consistency. The output is an updated merged dataset.

### Usage

The program likely requires specific input files or directories configured either internally or through environment variables (e.g., paths to the base dataset and the various update source datasets). Direct command-line arguments are not apparent.

### Example

*(No clear example can be derived from the provided context. Execution would depend on understanding the internal file path assumptions or required environment variables.)*

## min_neigh

This AWK script processes a neighbor correlation file (likely the output of `ushcn_corr.v5a.combo`) to filter stations based on the minimum number of neighbors they have above a certain correlation threshold. It reads the correlation file (which typically lists the target station followed by its neighbors and their correlations) and checks if the number of neighbors listed meets a minimum requirement (`MINSTN`). If a station meets the requirement, its ID is printed (presumably to create a new, filtered station list). It also adjusts a count of candidate stations (`ICAND`) if candidates are removed due to insufficient neighbors and writes a command to export the updated `icand` value to a `.cmd` file.

### Usage

```bash
awk -f min_neigh.awk -v MINSTN=<min_neighbors> -v ICAND=<initial_candidates> \
    <correlation_neighbor_file> > <filtered_station_list>
```

*   `MINSTN`: The minimum number of neighbors a station must have to be kept.
*   `ICAND`: The initial number of candidate stations in the input correlation file.
*   `<correlation_neighbor_file>`: The input file containing station IDs and their correlated neighbors (e.g., output from `ushcn_corr`).
*   `<filtered_station_list>`: The output file where the IDs of stations meeting the criteria will be written.
*   *(Implicit output)*: Creates `<correlation_neighbor_file>.cmd` containing `export icand=<updated_candidates>`.

### Example

```bash
awk -f min_neigh.awk -v MINSTN=5 -v ICAND=1218 correlation_output.txt > filtered_stations.txt
# Source the command file to update environment variable
# source correlation_output.txt.cmd
```

## newconf

This AWK script modifies a configuration file (likely for the PHA process) by decrementing the value associated with the `CONFIRM` key. It reads an input configuration file line by line. If a line starts with `CONFIRM=`, it subtracts 1 from the value and writes the modified line to the output file (`combination<NCOMBO+100>.txt`). All other lines are copied unchanged. The `NCOMBO` variable determines the output filename suffix.

### Usage

```bash
awk -f newconf.awk -v NCOMBO=<combo_number> <input_config_file>
```

*   `NCOMBO`: An integer used to generate the output filename (e.g., if `NCOMBO=1`, output is `combination101.txt`).
*   `<input_config_file>`: The input configuration file containing the `CONFIRM` key (e.g., `combination100.txt`).

### Example

```bash
awk -f newconf.awk -v NCOMBO=1 combination100.txt
```
*(This reads `combination100.txt`, decrements the `CONFIRM` value, and writes the result to `combination101.txt`)*.

## parse_ghcnd_stns

This AWK script parses a large GHCN-Daily data file (containing records for multiple stations, elements, and years) and splits it into individual station-element files compatible with the GHCNM 3-flag format. It reads the input data, determines the station ID, year, and element for each record, and appends the monthly data (value and flags) to the corresponding output file located in `<OUTDIR>/<TYPE>/<PROC>/<station_id>.<PROC>.<elem>`. It includes logic to handle different station ID formats (`hcnv1`, `coop`, `northam`) and filters stations based on region type (`TYPE`).

### Usage

```bash
awk -f parse_ghcnd_stns.awk -v OUTDIR=<output_directory> -v PROC=<proc_level> \
    -v TYPE=<region_type> <ghcnd_input_data_file>
```

*   `OUTDIR`: The base output directory.
*   `PROC`: The processing level identifier (e.g., `raw`).
*   `TYPE`: The region/network type being processed (e.g., `hcnv1`, `coop`, `northam`), which affects station ID parsing and filtering.
*   `<ghcnd_input_data_file>`: The large input data file containing records for multiple stations/elements.

### Example

```bash
awk -f parse_ghcnd_stns.awk -v OUTDIR=parsed_data -v PROC=raw \
    -v TYPE=coop ghcnd_all.dat
```

## stage3_to_ghcnm

This Fortran program converts data from the "Stage 3" merged format (likely the output of `merge_update` or a similar final merge step) into the standard GHCNM v4 format (11-char ID, year, 4-char element, 12x(value, 3 flags)). It reads the Stage 3 data file and inventory, likely processing station by station, and reformats the data records according to the GHCNM specification, writing the results to new output data and inventory files. It handles TMAX, TMIN, and TAVG elements.

### Usage

```bash
stage3_to_ghcnm <MAIN_DIRECTORY> <IN_METADATAFILE> <IN_DATADIR> \
                  <OUT_METADATAFILE> <OUT_DATAFILE> <VARIANT> <VERSION>
```

*   `<MAIN_DIRECTORY>`: Base directory containing resources and where output might be placed relative to standard structures.
*   `<IN_METADATAFILE>`: Path to the input Stage 3 inventory file.
*   `<IN_DATADIR>`: Directory containing the input Stage 3 data files (likely one per station).
*   `<OUT_METADATAFILE>`: Path for the output GHCNM-formatted inventory file.
*   `<OUT_DATAFILE>`: Path for the output GHCNM-formatted data file.
*   `<VARIANT>`: A 3-digit variant identifier (e.g., `000`).
*   `<VERSION>`: A version string (e.g., `v4.0.1`).

### Example

```bash
stage3_to_ghcnm /path/to/databank stage3.inv /path/to/stage3/data/ \
                  ghcnm.v4.0.1.inv ghcnm.v4.0.1.dat 000 v4.0.1
```

## summarize_Mv2_flags

This AWK script summarizes the usage of data flags within a GHCNMv2 or USHCN formatted data file. It reads the input file record by record, parses the 3-character flags associated with each monthly value, and counts the occurrences of each unique 3-character flag combination (separately for non-missing and missing data). It also counts the occurrences of each individual flag character (position 1, 2, and 3). Optionally (`LEVEL=detailed`), it can provide counts for specific data values associated with non-blank QC flags (position 2). The output is a summary report printed to standard output.

### Usage

```bash
awk -f summarize_Mv2_flags.awk -v FMT=<USHCN|GHCN> [-v LEVEL=detailed] <input_data_file>
```

*   `FMT`: Specifies the input data format (`USHCN` or `GHCN`), which affects column parsing.
*   `LEVEL=detailed` (Optional): Enables detailed counting of data values for specific QC flags.
*   `<input_data_file>`: The input data file in either GHCNMv2 or USHCN format.

### Example

```bash
# Summarize flags in a GHCNMv2 file
awk -f summarize_Mv2_flags.awk -v FMT=GHCN ghcnm_v2.dat > flag_summary.txt

# Detailed summary for a USHCN file
awk -f summarize_Mv2_flags.awk -v FMT=USHCN -v LEVEL=detailed ushcn_data.dat > flag_summary_detailed.txt
```

## twmgt2mthly

This AWK script appears intended to process time series data, possibly related to model output or gridded datasets (judging by the `HadSST3` example mentioned in comments), and convert it into a monthly format. However, the provided code seems incomplete or possibly corrupted, as the main processing block (`{...}`) and the `END` block contain logic (`read_data`, `print avg`) that conflicts with the variable names and structure defined in the `BEGIN` block and the function definitions (which seem copied from `average_network.awk`). It likely needs significant correction to function as intended. Assuming the goal was similar to averaging, it would read time series data and output monthly values.

### Usage

*(The script is likely non-functional as provided. Usage cannot be reliably determined.)*

### Example

*(No valid example can be provided due to the script's apparent inconsistencies.)*

## ushcn_corr.v5a.combo

This Fortran program calculates correlations between a "candidate" station and its nearest neighbors, identified previously by `ushcn_dist`. It reads station metadata, neighbor distance information, and the actual monthly time series data for the candidate and its potential neighbors. It computes the correlation between the candidate and each neighbor, potentially using first differences (`corr_type='1diff'`) or raw anomalies (`corr_type='near'` or other). The program then sorts the neighbors based on correlation strength (highest first) and applies filtering logic (minimum pairs, minimum neighbors per month/year, correlation threshold) to select the final set of neighbors for the candidate. The output is a file listing the candidate followed by its selected neighbors, their indices, and correlation values.

### Usage

```bash
ushcn_corr.v5a.combo <end_year> <element_code> -d <dist_file> -m <meta_file> \
                     -C <cand_dir> -N <net_dir> -o <outfile_base> \
                     -p <process_type> [-u <num_candidates>] [-n <num_output_neighbors>] [-6] [-D <debug_level>] [-X]
```

*   `<end_year>`: Last year of data to include in correlation calculation.
*   `<element_code>`: Integer code for the element (1=TMAX, 2=TMIN, 3=TAVG, 4=PRCP).
*   `-d <dist_file>`: Input file containing candidate IDs and their nearest neighbors by distance (output of `ushcn_dist`).
*   `-m <meta_file>`: Input metadata file for all stations (candidates and network).
*   `-C <cand_dir>`: Base directory for candidate station data files.
*   `-N <net_dir>`: Base directory for network station data files.
*   `-o <outfile_base>`: Base name for the output correlation file.
*   `-p <process_type>`: Processing stage identifier (e.g., `raw`, `tob`) used for locating data files.
*   `-u <num_candidates>` (Optional): Number of stations in `<meta_file>` that are candidates (vs. network).
*   `-n <num_output_neighbors>` (Optional): Overrides the default number of neighbors (`nstns`) to output.
*   `-6` (Optional): Use old 6-character ID format (legacy).
*   `-D <debug_level>` (Optional): Set debug print level (0=off).
*   `-X` (Optional): Turn off minimum monthly pair check.
*   *Environment Variables*: Reads `NEIGH_CLOSE`, `NEIGH_FINAL`, `NEIGH_CORR`, `CORR_LIM`, `MIN_STNS` for configuration.

### Example

```bash
# Set environment variables first
export NEIGH_CLOSE=40 NEIGH_FINAL=20 NEIGH_CORR=1diff CORR_LIM=0.1 MIN_STNS=7
# Run the program
ushcn_corr.v5a.combo 2010 1 -d neighbors_dist.txt -m all_stations.meta \
                     -C ./cand_data -N ./net_data -o neighbors_corr.txt \
                     -p raw -u 1218
```

## ushcn_dist.v6.combo

This Fortran program calculates the distances between "candidate" stations and a network of potential neighbor stations. It reads a metadata file containing IDs, latitudes, and longitudes for all stations (candidates and network members mixed, identified by the `-u` flag). For each candidate, it calculates the Great Circle Distance to all other *non-candidate* stations (and excluding known composite stations associated with the candidate if applicable). It finds the `ndist` closest neighbors within an expanding search radius (up to `distmax`), sorts them by distance, and writes the candidate ID, neighbor IDs, neighbor indices, and distances to an output file.

### Usage

```bash
ushcn_dist.v6.combo -m <meta_file> -o <outfile> [-u <num_candidates>] [-h] [-d <debug_level>]
```

*   `-m <meta_file>`: Input metadata file containing IDs, latitudes, and longitudes for all stations.
*   `-o <outfile>`: Output file path where neighbor distances will be written.
*   `-u <num_candidates>` (Optional): Number of stations at the beginning of `<meta_file>` that are candidates.
*   `-h` (Optional): Flag indicating non-HCN type candidates (affects metadata reading format slightly).
*   `-d <debug_level>` (Optional): Set debug print level (0=off).
*   *Environment Variable*: Reads `NEIGH_CLOSE` to determine the number of neighbors (`ndist`) to find and output.

### Example

```bash
# Set environment variable first
export NEIGH_CLOSE=40
# Run the program
ushcn_dist.v6.combo -m all_stations.meta -o neighbors_dist.txt -u 1218
```

## ushcn_fill.v4p

This Fortran program fills missing data points in a time series using the FILNET methodology, which relies on a network of neighboring stations. It reads metadata for the candidate station and its pre-selected network (from `ushcn_corr`), reads the candidate's data (potentially with existing filled values or flags) and the network stations' data. For each missing data point in the candidate series, it uses the corresponding data from the optimal subset of the neighbor network (determined dynamically based on data availability and potentially confidence intervals) to estimate a value for the missing point. The output is the candidate station's data file with missing values filled and appropriate flags updated ('E' for estimated).

### Usage

```bash
ushcn_fill.v4p -c <cand_meta> -n <cand_net_file> -p <cand_proc> -i <cand_conf_proc> \
               -q <net_proc> -o <out_proc> -j <out_conf_proc> -e <elem_code> \
               -C <cand_dir> -N <net_dir> [-u <num_candidates>] [-k] [-d <debug_level>]
```

*   `-c <cand_meta>`: Candidate station metadata file.
*   `-n <cand_net_file>`: Candidate-network definition file (output from `ushcn_corr`).
*   `-p <cand_proc>`: Processing level for candidate input data.
*   `-i <cand_conf_proc>`: Processing level for candidate confidence interval input (if used).
*   `-q <net_proc>`: Processing level for network input data.
*   `-o <out_proc>`: Processing level identifier for the output filled data file.
*   `-j <out_conf_proc>`: Processing level identifier for the output confidence interval file (if generated).
*   `-e <elem_code>`: Integer code for the element (1=TMAX, 2=TMIN, 3=TAVG, 4=PRCP).
*   `-C <cand_dir>`: Base directory for candidate data files.
*   `-N <net_dir>`: Base directory for network data files.
*   `-u <num_candidates>` (Optional): Number of candidate stations in meta/network files.
*   `-k` (Optional): Keep suspect data (default is to remove/fill).
*   `-d <debug_level>` (Optional): Set debug print level (0=off).

### Example

```bash
ushcn_fill.v4p -c candidates.meta -n neighbors_corr.txt -p WMs.52d -i WMs.52d \
               -q WMs.52d -o FLs.52d -j FLs.52d -e 1 \
               -C ./cand_data -N ./net_data -u 1218
```

# Data formats

## GHCNM Inventory Format (`.inv`, `meta.*`)

**Description:**
This format stores station metadata, including station ID, geographical coordinates (latitude, longitude, elevation), station name, and potentially other information like composite station IDs or period of record indicators depending on the specific variant or generating program. It serves as the master list of stations for various processing steps.

**Syntax (GHCNMv2/v3 Style - used by `test-station-meta-v3.txt`):**

| Columns  | Width | Description                                      | Example         |
|----------|-------|--------------------------------------------------|-----------------|
| 1-11     | 11    | Station ID (11 characters)                       | `USH00018323`   |
| 13-20    | 8     | Latitude (decimal degrees, F8.4)                 | `31.8075`       |
| 22-30    | 9     | Longitude (decimal degrees, F9.4)                | `-85.9722`      |
| 32-37    | 6     | Elevation (meters, F6.1)                         | `165.2`         |
| 39-68    | 30    | Station Name                                     | `TROY`          |
| 70-80    | 11    | Composite/Source ID 1 (or other metadata)        | `USC00018323`   |
| 82-92    | 11    | Composite/Source ID 2 (or other metadata)        | `-----------`   |
| 94-104   | 11    | Composite/Source ID 3 (or other metadata)        | `-----------`   |
| 106-116  | 11    | Composite/Source ID 4 (or other metadata)        | `-----------`   |
| 118-120  | 3     | Other Flags/Metadata                             | `+6`            |
| 121      | 1     | Asterisk Flag (Added by `ghcnm_clean`)           | `*`             |

*(Note: The exact content beyond column 68 can vary depending on the file's origin. `gen_meta.awk` output has element POR info here).*

**Input Programs:**
*   `convert.pha2cas.awk` (Reads inventory to select stations and metadata)
*   `gen_meta.awk` (Reads a master inventory like `ghcnm.inv`)
*   `join_gv4.awk` (Reads inventory or `gen_meta` output)
*   `ushcn_dist.v6.combo.f` (Reads combined candidate/network metadata)
*   `ushcn_corr.v5a.combo.f` (Reads combined candidate/network metadata)
*   `ushcn_fill.v4p.f` (Reads reference station metadata via `readrsta`)
*   `ghcnm_clean` (Reads input inventory)
*   `ghcnm_edit` (Reads input inventory)
*   `ghcnm_qc` (Reads input inventory)
*   `stage3_to_ghcnm` (Reads Stage 3 inventory)

**Output Programs:**
*   `convert.pha2cas.awk` (Writes `OUTINV` file)
*   `gen_meta.awk` (Writes element-specific meta files, e.g., `ghcn.meta.tmax`)
*   `join_gv4.awk` (Writes `OUTINV` file)
*   `ghcnm_clean` (Writes cleaned output inventory)
*   `ghcnm_edit` (Writes edited output inventory)
*   `stage3_to_ghcnm` (Writes GHCNM-formatted output inventory)

**Example (`test-station-meta-v3.txt`):**
```
USH00018323  31.8075  -85.9722  165.2 AL TROY                            USC00018323 ----------- ----------- ----------- +6
10160475000  35.4800    8.1300   67.0 DUMMY-STATION
```

---

## GHCNM 3-Flag Monthly Data Format (`.dat`, `.raw.*`, `.tob.*`, `.adj.*`, etc.)

**Description:**
This is the primary format for storing monthly climate data (TMAX, TMIN, TAVG, PRCP, TDTR) used and produced by many programs in this suite. Each line represents one year of data for a specific station and element. It includes the station ID, year, and 12 monthly values, each accompanied by three flags: Data Measurement (DM), Quality Control (QC), and Data Source (DS).

**Syntax:**

| Columns | Width | Description                               | Example         |
|---------|-------|-------------------------------------------|-----------------|
| 1-11    | 11    | Station ID                                | `USH00018323`   |
| 12-12   | 1     | Space (sometimes element code in older F77) |                 |
| 13-16   | 4     | Year (YYYY)                               | `1950`          |
| 17-25   | 9     | January Value (I6) + Flags (A3)           | `  1368a  `     |
| 26-34   | 9     | February Value (I6) + Flags (A3)          | `  1754   `     |
| 35-43   | 9     | March Value (I6) + Flags (A3)             | `  2125   `     |
| 44-52   | 9     | April Value (I6) + Flags (A3)             | `  2512   `     |
| 53-61   | 9     | May Value (I6) + Flags (A3)               | `  2762   `     |
| 62-70   | 9     | June Value (I6) + Flags (A3)              | `  3818   `     |
| 71-79   | 9     | July Value (I6) + Flags (A3)              | `  3844   `     |
| 80-88   | 9     | August Value (I6) + Flags (A3)            | `  3757   `     |
| 89-97   | 9     | September Value (I6) + Flags (A3)         | `  3411   `     |
| 98-106  | 9     | October Value (I6) + Flags (A3)           | ` -9999   `     |
| 107-115 | 9     | November Value (I6) + Flags (A3)          | `  2532   `     |
| 116-124 | 9     | December Value (I6) + Flags (A3)          | `  2283   `     |

*Value: Integer, typically hundredths of degrees C or tenths of mm. -9999 indicates missing.*
*Flags: DM QC DS (e.g., `E  `, ` X `, ` S `)*

**Input Programs:**
*   `average_network.awk` (Reads individual station files)
*   `average_stn.awk` (Reads individual station files)
*   `convert.pha2cas.awk` (Reads GHCN/USHCN data files specified by `GDIR`/`UDIR`)
*   `gen_avg.3flg.awk` (Reads `tmax`, `tmin` files)
*   `gen_avg_dtr.3flg.awk` (Reads `tmax`, `tmin` files)
*   `gen_dtr.3flg.awk` (Reads `tmax`, `tmin` files)
*   `gen_meta.awk` (Reads station data files to check POR and observation counts)
*   `join_gv4.awk` (Reads GHCN data files specified by `GDIR`)
*   `count_edit_pha.awk` (Reads PHA output file)
*   `count_obs.awk` (Reads station data file)
*   `ushcn_corr.v5a.combo.f` (Reads candidate and network data via `getdata`)
*   `ushcn_fill.v4p.f` (Reads candidate and network data via `readsta`/`filinit`)
*   `ushcn_tobs.v6.f` (Reads raw input data)
*   `PHAMain` / `ReadInputFiles` (Reads element data)
*   `ghcnm_clean` (Reads input data file)
*   `ghcnm_edit` (Reads input data file)
*   `ghcnm_climat` (Reads input data file)
*   `ghcnm_qc` (Reads input data file)

**Output Programs:**
*   `convert_mv2_d2m.awk` (Writes individual station-element files)
*   `gen_avg.3flg.awk` (Writes `tavg` file)
*   `gen_avg_dtr.3flg.awk` (Writes `tavg` or `tdtr` file)
*   `gen_dtr.3flg.awk` (Writes `tdtr` file)
*   `parse_ghcnd_stns.awk` (Splits large file into this format)
*   `ushcn_fill.v4p.f` (Writes filled candidate data via `writsta`)
*   `ushcn_tobs.v6.f` (Writes TOB-adjusted data)
*   `PHAMain` / `AdjustSeries` (Writes final adjusted data)
*   `gen-mon-composites.v2.f95` (Writes composite station data)
*   `ghcnm_clean` (Writes cleaned output data)
*   `ghcnm_edit` (Writes edited output data)
*   `ghcnm_climat` (Writes updated output data)
*   `ghcnm_qc` (Writes data with updated QC flags)

**Example (Conceptual):**
```
USH00018323 1917  1368a    1754     2125     2512     2762     3818     3844     3757     3411    -9999     2532     2283
USH00018323 1919 -9999 X  -9999 X  -9999 X  -9999 X  -9999    -9999 X  -9999 Q  -9999 X   3528f    2523f   -9999     1710
```

---

## GHCNMv2 Monthly Data Format

**Description:**
This format, used by older versions of GHCNM, stores monthly data similarly to the 3-flag format but uses a different fixed-width layout and includes a 4-character element identifier within each record. Values are typically 5 digits. Several AWK scripts (`compare.mm_avg.awk`, `convert_mv2_d2m.awk`, `summarize_Mv2_flags.awk`) are designed to read or process this format.

**Syntax:**

| Columns | Width | Description                       | Example         |
|---------|-------|-----------------------------------|-----------------|
| 1-11    | 11    | Station ID                        | `10160475000`   |
| 12-15   | 4     | Year (YYYY)                       | `1950`          |
| 16-19   | 4     | Element Code (e.g., `TMAX`, `TAVG`) | `TAVG`          |
| 20-27   | 8     | January Value (I5) + Flags (A3)   | `  123   `      |
| 28-35   | 8     | February Value (I5) + Flags (A3)  | `  234   `      |
| ...     | ...   | ...                               | ...             |
| 108-115 | 8     | December Value (I5) + Flags (A3)  | ` -105   `      |

**Input Programs:**
*   `compare.mm_avg.awk`
*   `convert_mv2_d2m.awk`
*   `summarize_Mv2_flags.awk` (Can be configured for this via `FMT=GHCN`)

**Output Programs:**
*   *(None in this specific list directly output this exact format, but `stage3_to_ghcnm` outputs a similar GHCNMv4 format)*

**Example (Conceptual):**
```
10160475000 1950 TAVG   123      234      345      456      567      678      789      890      901      802      403     -105
```

---

## QCA Monthly Data Format (?)

**Description:**
Referenced by `count_edit_qc.awk`. This format seems similar to GHCNMv2 but potentially with slightly different column positions for year and data/flags. It's likely an intermediate format produced by an earlier QC step before being converted or used elsewhere.

**Syntax (Inferred from `count_edit_qc.awk`):**

| Columns | Width | Description                       | Example         |
|---------|-------|-----------------------------------|-----------------|
| 1-11    | 11    | Station ID                        | `10160475000`   |
| 12-15   | 4     | Year (YYYY)                       | `2014`          |
| 16-19   | 4     | Element Code (e.g., `TMAX`, `TAVG`) | `TAVG`          |
| 20-27   | 8     | January Value (I5) + Flags (A3)   | `  123 Q `      |
| 28-35   | 8     | February Value (I5) + Flags (A3)  | `  234   `      |
| ...     | ...   | ...                               | ...             |
| 108-115 | 8     | December Value (I5) + Flags (A3)  | ` -105 X `      |

**Input Programs:**
*   `count_edit_qc.awk`

**Output Programs:**
*   *(Likely produced by an earlier QC program not included in the list, possibly `ghcnm_qc` before format conversion)*

**Example (Conceptual):**
```
10160475000 2014 TAVG   123 Q    234      345      456      567 X    678      789      890      901      802      403      -105
```

---

## Neighbor Distance File Format

**Description:**
This file stores the nearest neighbors for each target station, ordered by distance. Each target station has three corresponding lines: neighbor IDs, neighbor indices (pointers), and distances.

**Syntax (3 lines per target station):**
1.  `TargetID NeighborID1 NeighborID2 ... NeighborIDN` (Space-separated IDs, A11)
2.  `TargetIndex NeighborIndex1 NeighborIndex2 ... NeighborIndexN` (Space-separated integers, I11)
3.  `0.0 Distance1 Distance2 ... DistanceN` (Space-separated reals, F11.1)

**Input Programs:**
*   `ushcn_corr.v5a.combo.f` (Reads neighbor IDs and indices)

**Output Programs:**
*   `ushcn_dist.v6.combo.f`
*   `ChooseNeighbors` (Fortran 95 module)

**Example:**
```
USH00018323 USH00018380 10160475000 10160490000 10160518000 10160522000
          1           2           3           4           5           6
        0.0       205.4       230.9       790.0       859.6       904.0
```

---

## Neighbor Correlation File Format

**Description:**
Similar to the distance file, but stores the "best" neighbors selected based on correlation (after initial distance filtering). Neighbors are ordered by correlation strength (highest first). Each target station has three corresponding lines: neighbor IDs, neighbor indices, and correlation values.

**Syntax (3 lines per target station):**
1.  `TargetID NeighborID1 NeighborID2 ... NeighborIDN` (Space-separated IDs, A11)
2.  `TargetIndex NeighborIndex1 NeighborIndex2 ... NeighborIndexN` (Space-separated integers, I11)
3.  `1.00 Correlation1 Correlation2 ... CorrelationN` (Space-separated reals, F11.2)

**Input Programs:**
*   `ushcn_fill.v4p.f` (Reads candidate-network definitions)
*   `min_neigh.awk` (Filters stations based on number of neighbors)
*   `ReadInputFiles` (Fortran 95 module, reads neighbor IDs and indices)

**Output Programs:**
*   `ushcn_corr.v5a.combo.f`
*   `ChooseNeighbors` (Fortran 95 module)

**Example:**
```
USH00018323 10160475000 USH00018380 10160522000 10160490000 10160518000
          1           3           2           6           4           5
       1.00        0.85        0.76        0.65        0.55        0.45
```

---

## Properties File Format (`.properties`)

**Description:**
Standard Java-style properties files containing key-value pairs, separated by `=`. Lines starting with `#` are comments. These files configure the behavior of the main PHA Fortran programs (`PHAMain`, `PHATestOutput`, `PHATestUnits`). Keys often use a hierarchical dot notation (e.g., `pha.logger.filename`). Values can reference other properties using `{key_name}` syntax or command-line arguments using `{arg:flag}` syntax, which are resolved at runtime.

**Syntax:**
```
# This is a comment
key1 = value1
pha.path.station.history = /data/history/
pha.logger.filename = logs/pha_{pha.element}.log
pha.test.use-arg = use-{arg:d}
```

**Input Programs:**
*   `PHAMain` (Reads main configuration)
*   `PHATestOutput` (Reads test configuration)
*   `PHATestUnits` (Reads unit test configuration)
*   *(Internally uses `PropertyReader.f95` module)*

**Output Programs:**
*   *(None directly output `.properties` files, but `newconf.awk` modifies one)*

**Required Keys (Best Guess based on `ghcnm-pha.test.properties` and code):**

*   **Logger Configuration:**
    *   `pha.logger.filename`: Path to the log file.
    *   `pha.logger.level`: Logging level (DEBUG, INFO, WARN, ERROR, FATAL).
    *   `pha.logger.print-to-stdout`: `true` or `false`.
    *   `pha.logger.append-datestamp`: `true` or `false`.
    *   `pha.logger.rollover-datestamp`: `true` or `false`.
*   **Core PHA Run Parameters:**
    *   `pha.begin-year`: Earliest year to consider in data.
    *   `pha.element`: Element being processed (tmax, tmin, tavg, tdtr).
    *   `pha.input-data-type`: Input data stage (e.g., `raw`, `tob`).
    *   `pha.neighbors.input-data-type`: Input data stage for neighbor data.
    *   `pha.version`: Version identifier for output files/directories.
*   **File Path Configuration:**
    *   `pha.path.station-metadata`: Path to the station inventory file.
    *   `pha.path.neighbors.distance`: Path to the neighbor-distance file (output).
    *   `pha.path.neighbors.correlation`: Path to the neighbor-correlation file (output).
    *   `pha.path.neighbors.correlation-in`: Path to the neighbor-correlation file (input).
    *   `pha.path.station-element-data-in`: Path template for input element data files.
    *   `pha.path.neighbors.station-element-data-in`: Path template for neighbor input element data files.
    *   `pha.path.station-element-data-out`: Path template for output adjusted data files.
    *   `pha.path.station-history`: Directory containing station history (`.his`) files.
*   **Neighbor Selection Parameters:**
    *   `pha.neighbors.distance-neighbor-limit`: Initial number of distance neighbors to consider.
    *   `pha.neighbors.method`: Method for correlation (`first-diffs`, `monthly-anomaly`, `distance-only`).
    *   `pha.neighbors.min-coefficient`: Minimum correlation coefficient threshold.
    *   `pha.neighbors.min-station-coverage`: Minimum number of neighbors needed for spatial coverage checks.
    *   `pha.neighbors.final-neighbor-limit`: Maximum number of neighbors in the final correlation list.
*   **Changepoint/Adjustment Parameters:**
    *   `pha.use-history-files`: How to use station history (0=ignore, 1=use+detect, -1=only).
    *   `pha.snht-threshold`: SNHT significance level (1, 5, or 10).
    *   `pha.bic-penalty`: BIC penalty type (`bic`, `aic`, `none`).
    *   `pha.amploc-percent`: Confidence limit for timing uncertainty (90, 92, 95).
    *   `pha.confirm`: Minimum coincident pairwise changepoints to confirm a target changepoint.
    *   `pha.adjust.min-length`: Minimum segment length (months) for adjustment.
    *   `pha.adjust.min-neighbors`: Minimum neighbors needed for adjustment estimate.
    *   `pha.adjust.remove-outliers`: `true` or `false` for Tukey outlier removal.
    *   `pha.adjust.window`: Window size (months) for adjustment calculation (0=no limit).
    *   `pha.adjust.filter-method`: Outlier filtering method (`conf`, `bicf`, `both`, `none`).
    *   `pha.adjust.est-method`: Adjustment estimation method (`med`, `avg`, `qav`).
    *   `pha.remove-insignificant`: `true` or `false` to merge non-significant breaks.
*   **Test-Specific Parameters (Examples):**
    *   `pha.test.*`: Various keys used specifically within `ConfigurationUtilsTest.f95`.
    *   `pha.do-run-main`, `pha.do-run-neighbors`: Control execution flow in test programs.
    *   `pha.path.*-expected*`: Paths to expected output files for comparison tests.

**Example (`ghcnm-pha.unit-test.properties` snippet):**
```properties
# Properties file specifically for running PHATestUnits

# === Logger Configuration ===
pha.logger.filename = build/pha-unit-test.log
pha.logger.level = DEBUG

# === Core PHA Run Parameters ===
pha.begin-year = 1851
pha.element = tmax
pha.input-data-type = raw
pha.version = uni

# === File Path Configuration ===
pha.path.station-metadata = data/ghcnm.inv
pha.path.neighbors.distance = output/neighbors-distance.unit-test.txt

# === Neighbor Selection Parameters ===
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.method = first-diffs
```

---

## Station History File Format (`.his`)

**Description:**
These files contain documented changes for a specific station, such as moves, instrument changes, or observation time changes. Each line represents a period of record with consistent metadata. The PHA process can use this information to help identify or confirm changepoints. The format seems complex and potentially varies based on the source (`source` field).

**Syntax (Inferred from `ushcn_tobs.v6.f` READ statement):**

| Columns | Width | Description                                      | Example         |
|---------|-------|--------------------------------------------------|-----------------|
| 1-1     | 1     | Source Code (0=USHCN, 1=Daily TOB?, 2=MSHR, 3=CDMP)| `0`             |
| 14-17   | 4     | Begin Year (YYYY)                                | `1948`          |
| 19-20   | 2     | Begin Month (MM)                                 | `07`            |
| 21-22   | 2     | Begin Day (DD)                                   | `01`            |
| 24-27   | 4     | End Year (YYYY)                                  | `1950`          |
| 29-30   | 2     | End Month (MM)                                   | `06`            |
| 31-32   | 2     | End Day (DD)                                     | `30`            |
| 34-36   | 3     | Latitude Degrees                                 | `35`            |
| 37-39   | 3     | Latitude Minutes                                 | `05`            |
| 40-42   | 3     | Latitude Seconds                                 | `00`            |
| 44-47   | 4     | Longitude Degrees                                | `-89`           |
| 48-50   | 3     | Longitude Minutes                                | `58`            |
| 51-53   | 3     | Longitude Seconds                                | `00`            |
| 55-65   | 11    | Distance/Direction from previous location        | `001 NNE    `   |
| 67-71   | 5     | Elevation (units depend on source?)              | `165`           |
| 74-77   | 4     | Instrument Height (e.g., Precip/Temp)            | `0506`          |
| 79-82   | 4     | Observation Time Code                            | `18HR`          |
| 88-142  | 55    | Instrument Codes (11 x A5, space-separated)      | `CRS MXMN ...`  |

**Input Programs:**
*   `ushcn_tobs.v6.f` (Reads history to determine TOB adjustments)
*   `ReadInputFiles` (Fortran 95 module, reads history for PHA)

**Output Programs:**
*   *(None - These are typically external inputs)*

**Example (Conceptual):**
```
0            1948 07 01 1950 06 30  35 05 00  -89 58 00 001 NNE       165  0506 18HR     CRS   MXMN  HYTHG SG    SRG   SS    TG    NSRG  RRNG  RRIG  FP
```

---

## Data Edit File Format (`edit.dat`)

**Description:**
This file provides instructions for editing specific data points in the GHCNM dataset. Each line specifies a station, element, date, the value to potentially change, and the new flags. A code ('D' or 'C') indicates whether the change should only occur if the original data matches the value in the edit file ('D' - Delay until new value?) or if the change should be applied unconditionally ('C' - Correct).

**Syntax (Inferred from `ghcnm_edit.f95` READ statement):**

| Col | Width | Description                               | Example         |
|-----|-------|-------------------------------------------|-----------------|
| 1-1 | 1     | Edit Code ('C' or 'D')                    | `C`             |
| 3-13| 11    | Station ID                                | `10160475000`   |
| 15-18| 4     | Element Code (e.g., `TMAX`)               | `TMAX`          |
| 20-23| 4     | Year (YYYY)                               | `1980`          |
| 25-26| 2     | Month (MM)                                | `05`            |
| 28-32| 5     | Value (Integer, units match data file)    | ` 1500`         |
| 34-34| 1     | New DM Flag                               | ` `             |
| 36-36| 1     | New QC Flag                               | `X`             |
| 38-38| 1     | New DS Flag                               | ` `             |

**Input Programs:**
*   `ghcnm_edit`

**Output Programs:**
*   *(None - This is an input control file)*

**Example (Conceptual):**
```
C 10160475000 TMAX 1980 05  1500   X
D 10160490000 TAVG 1995 11 -9999 M S Z
```

---

## Metadata Edit File Format (`ghcnmv4_edit_meta.txt`)

**Description:**
This file provides instructions for updating station metadata (name, latitude, longitude, elevation). Each line specifies the station ID and the new metadata values to apply.

**Syntax (Inferred from `ghcnm_edit.f95` READ statement):**

| Columns | Width | Description                       | Example         |
|---------|-------|-----------------------------------|-----------------|
| 1-11    | 11    | Station ID                        | `10160475000`   |
| 13-60   | 48    | (Skipped Columns)                 |                 |
| 61-90   | 30    | New Station Name                  | `NEW STATION NAME` |
| 92-101  | 10    | New Latitude (Decimal Deg, F10.4) | `  35.5000`     |
| 103-112 | 10    | New Longitude (Decimal Deg, F10.4)| `   8.2500`     |
| 114-121 | 8     | New Elevation (Meters?, F8.2)     | `   70.00`      |

**Input Programs:**
*   `ghcnm_edit`

**Output Programs:**
*   *(None - This is an input control file)*

**Example (Conceptual):**
```
10160475000                                                NEW STATION NAME                35.5000     8.2500    70.00
```

---

## CLIMAT Message File Format

**Description:**
This format contains raw CLIMAT messages, likely interspersed with other text or headers. The `climat_decoder` program specifically looks for lines containing indicators like `CLIMAT`, `111`, `222`, `333`, `444`, and group codes like ` 3`, ` 4`, ` 6` within section `111` to extract WMO ID, date, and specific meteorological values (TAVG, TMAX, TMIN, PRCP). The exact structure is complex and follows WMO standards for CLIMAT reports.

**Syntax:**
*(Complex and variable, based on WMO CLIMAT code form. Parsing relies on finding specific keywords and group indicators within lines).*

**Input Programs:**
*   `climat_decoder`

**Output Programs:**
*   *(None - This is typically raw input from meteorological data streams)*

**Example (Conceptual Snippet):**
```
CLIMAT 05/14 12345
111 01234 30123 40123 61000 ... =
```

---

## Log File Format (`.log`)

**Description:**
Plain text files used for logging program execution details, warnings, errors, and debug information. The format typically includes a timestamp, log level indicator (e.g., INFO, WARN, ERROR), and the log message. Configuration is controlled by `pha.logger.*` properties.

**Syntax (Typical):**
`YYYY-MM-DD HH:MM:SS LEVEL Message Text`

**Input Programs:**
*   *(None - Primarily for human inspection)*

**Output Programs:**
*   `PHAMain` (via `Logger.f95`)
*   `PHATestOutput` (via `Logger.f95`)
*   `PHATestUnits` (via `Logger.f95`)

**Example:**
```
2024-03-17 10:30:01 INFO BEGIN run of GHCN-Monthly PHA
2024-03-17 10:30:01 INFO Using properties file: ghcnm-pha.properties
2024-03-17 10:30:02 DEBUG Reading station metadata from data/ghcnm.inv
2024-03-17 10:35:15 WARN Station USH00012345 has insufficient data overlap with neighbor USC0098765
```
