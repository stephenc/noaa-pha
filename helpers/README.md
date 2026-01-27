# GHCNM PHA Helper Tools

Python utilities for working with GHCNM v3/v52i and v4 data formats.

## Prerequisites

**For convert_ghcnm_to_v4.py and convert_v3_to_v4.py:** Standard Python 3 libraries only (no external dependencies)

**For plot_compare.py and plot_compare_average.py:** Requires matplotlib

Install dependencies using `uv`:
```bash
cd helpers
uv sync
```

Or install manually:
```bash
pip install matplotlib numpy
```

## convert_ghcnm_to_v4.py

Converts NOAA GHCN-Monthly v4 distribution format to PHAMain input format.

**Key Features:**
- Processes official NOAA GHCNM v4 data downloads (.qcu.tar.gz files)
- Splits monolithic .dat file into individual station files
- Converts NOAA format to PHAMain 3-flag input format
- Creates complete v4 directory structure automatically
- Supports all elements: TAVG, TMAX, TMIN, TDTR

### NOAA Distribution Format

**Station Inventory (.inv file):**
- Already in correct v4 format (used as-is)
- Fixed-width format with ID, lat/lon, elevation, name

**Data File (.dat file):**
- Single monolithic file containing ALL stations
- Format: `ID(11) YEAR(4) ELEMENT(4) VALUE1(5) FLAGS1(3) ... VALUE12(5) FLAGS12(3)`
- Example: `ACW000112101931TAVG -9999     -9999     -9999     -9999    ...`

### PHAMain Input Format

**Individual station files:**
- One file per station per element: `{station_id}.raw.{element}`
- Format: `ID(11) SPACE YEAR(4) VALUE1(6) FLAGS1(3) ... VALUE12(6) FLAGS12(3)`
- Example: `ACW00011210 1931 -9999    -9999    -9999    -9999    ...`

### Usage

```bash
python3 convert_ghcnm_to_v4.py INPUT_DIR OUTPUT_DIR [-v]
```

**What it does:**
- Finds .inv and .dat files in INPUT_DIR
- Copies ghcnm.inv to OUTPUT_DIR
- Splits .dat file into individual station files in OUTPUT_DIR/data/raw/
- Creates complete directory structure:
  - `OUTPUT_DIR/ghcnm.inv` (station inventory)
  - `OUTPUT_DIR/data/raw/` (individual station files)
  - `OUTPUT_DIR/output/adjusted/{element}/` (for PHA output)
  - `OUTPUT_DIR/history/` (for station history files)
  - `OUTPUT_DIR/output/neighbors/` (for neighbor files)

### Complete Example: Processing Official GHCNM v4 Data

```bash
# 1. Download latest GHCNM v4 data from NOAA
wget ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tavg.latest.qcu.tar.gz

# 2. Extract the archive
tar xzf ghcnm.tavg.latest.qcu.tar.gz
# This creates directory like: ghcnm.v4.0.1.20260126/

# 3. Convert to PHAMain format (use -v to see progress)
python3 helpers/convert_ghcnm_to_v4.py ./ghcnm.v4.0.1.20260126 ./ghcnm-data -v

# This processes:
# - ~28,000 stations worldwide
# - All available TAVG records
# - Preserves all quality control flags
# - Opens/writes/closes files immediately (no file handle limits)
```

**After conversion, your directory structure will be:**
```
ghcnm-data/
├── ghcnm.inv                          (station inventory)
├── data/
│   └── raw/
│       ├── ACW00011604.raw.tavg
│       ├── ACW00011647.raw.tavg
│       └── ... (one file per station)
├── history/                           (empty, for .his files)
└── output/
    ├── neighbors/                     (empty, PHAMain will create)
    └── adjusted/
        └── tavg/                      (empty, for PHAMain output)
```

### Configure PHAMain Properties File

After conversion, a complete properties file `ghcnm-pha.properties` is provided in the repository root for processing NOAA GHCNM v4 data:

```bash
# Run PHAMain with the provided configuration
./bin/PHAMain

# Or specify a custom properties file
./bin/PHAMain -p my-custom.properties
```

The `ghcnm-pha.properties` file includes:
- **Logger settings**: Control output verbosity and log file location
- **Core parameters**: Begin year (1850), element (tavg), version tag (ghcn)
- **File paths**: All paths configured for ghcnm-data/ directory structure
- **Neighbor selection**: 40 distance neighbors, first-diffs method, 20 final neighbors
- **Changepoint detection**: SNHT threshold 5%, confirmation level 3
- **Adjustment parameters**: Min length 18 months, min neighbors 5, median estimation

**Key settings from v52i:**
```properties
pha.snht-threshold = 5        # v52i: -Q 1.46
pha.adjust.min-length = 18    # v52i: -S 18
pha.adjust.min-neighbors = 5  # v52i: -s 5
pha.use-history-files = 0     # v52i: -P (set to 0 without custom histories)
```

To process a different element (tmax or tmin), edit the properties file:
```properties
pha.element = tmax  # or tmin
pha.path.station-element-data-out = ghcnm-data/output/adjusted/tmax/
```

### Processing Multiple Elements

To process TMAX and TMIN in addition to TAVG:

```bash
# Download and extract each element
wget ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tmax.latest.qcu.tar.gz
wget ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tmin.latest.qcu.tar.gz

tar xzf ghcnm.tmax.latest.qcu.tar.gz
tar xzf ghcnm.tmin.latest.qcu.tar.gz

# Convert each dataset (they can share the same output directory)
python3 helpers/convert_ghcnm_to_v4.py ./ghcnm.v4.0.1.tmax.20260126 ./ghcnm-data -v
python3 helpers/convert_ghcnm_to_v4.py ./ghcnm.v4.0.1.tmin.20260126 ./ghcnm-data -v
```

The converter automatically creates element-specific directories and handles multiple elements in the same output directory.

### Validation

Check the converted files:

```bash
# Check inventory format
head -5 ghcnm-data/ghcnm.inv

# Count converted files by element
ls ghcnm-data/data/raw/*.raw.tavg | wc -l
ls ghcnm-data/data/raw/*.raw.tmax | wc -l
ls ghcnm-data/data/raw/*.raw.tmin | wc -l

# Check data format (should have space in column 12, 6-char values, 3-char flags)
head -3 ghcnm-data/data/raw/ACW00011604.raw.tavg

# Verify no duplicate station IDs
cut -c1-11 ghcnm-data/ghcnm.inv | sort | uniq -d
```

## convert_v3_to_v4.py

Converts GHCNM v3/v52i format data to GHCNM v4 format.

**Key Features:**
- Converts station lists and monthly data files
- Creates complete v4 directory structure automatically
- Creates all required directories for PHAMain (output/, history/, etc.)

### Format Differences

**v3/v52i Station List:**
- Format: `ID Lat Lon TZ1 TZ2 Elev Name`
- Example: `PW100010148  34.23    -86.17   +6 01  348    ALBERTVILLE 2 SE`

**v4 Station Inventory:**
- Format: Fixed-width columns
- Columns 1-11: Station ID
- Columns 13-20: Latitude (F8.4)
- Columns 22-30: Longitude (F9.4)
- Columns 32-37: Elevation (F6.1)
- Columns 39+: Station Name
- Example: `PW100010148  34.2300  -86.1700  348.0 ALBERTVILLE 2 SE`

**v3/v52i Monthly Data:**
- Format: `ID Year Value1 Value2 ... Value12` (no flags)
- Example: `PW100010148 1900 -9999 -9999 -9999 1703 1971 2351 2848 2738 2365 1747 1164 747`

**v4 Monthly Data:**
- Format: `ID Year Value1FFF Value2FFF ... Value12FFF` (3 flags per value)
- Example: `PW100010148 1900 -9999    -9999    -9999     1703     1971     2351     2848     2738     2365     1747     1164      747   `

### Usage

#### Full Dataset Conversion (Recommended)

Convert an entire v3 dataset with one command:

```bash
python3 convert_v3_to_v4.py INPUT_BASE OUTPUT_BASE [--element tavg] [-v]
```

**What it does:**
- Converts station list from `INPUT_BASE/meta/world1_stnlist.{element}`
- Converts all data files from `INPUT_BASE/monthly/raw/*.raw.{element}`
- Creates complete v4 directory structure:
  - `OUTPUT_BASE/ghcnm.inv` (station inventory)
  - `OUTPUT_BASE/data/raw/` (converted data files)
  - `OUTPUT_BASE/output/adjusted/{element}/` (for PHA output)
  - `OUTPUT_BASE/history/` (for station history files)

**Example:**
```bash
python3 convert_v3_to_v4.py pw-data-v3/benchmark/world1 pw-data-v4 --element tavg -v
```

**Parameters:**
- `INPUT_BASE`: Directory containing `meta/` and `monthly/` subdirectories
- `OUTPUT_BASE`: Output directory (will be created)
- `--element`: Element type (default: `tavg`, options: `tmax`, `tmin`)
- `--station-list`: Station list filename (default: `world1_stnlist.tavg`)
- `--flags`: Flag characters to add (default: 3 spaces)
- `-v`: Verbose output

### Complete Example: Converting Peter's World

Peter's World is a benchmark dataset from the v52i branch containing 7720 stations:

```bash
# 1. Extract the benchmark data
tar xzf benchmark.tar.gz -C pw-data-v3

# 2. Convert entire dataset with one command
python3 helpers/convert_v3_to_v4.py \
    pw-data-v3/benchmark/world1 \
    pw-data-v4 \
    --element tavg \
    -v
```

This converts:
- 7,720 stations
- 437,457 monthly records
- Creates all required directories for PHAMain

### Validation

Check the converted files:

```bash
# Check inventory format (should be fixed-width)
head -5 world1.inv | cat -A

# Check data format (should have 3 spaces after each value)
head -3 /tmp/world1_v4_converted/PW100010148.raw.tavg

# Count converted files
ls /tmp/world1_v4_converted/*.raw.tavg | wc -l

# Verify station IDs match
cut -c1-11 world1.inv | sort > inv_ids.txt
ls /tmp/world1_v4_converted/*.raw.tavg | \
    xargs -n1 basename | \
    cut -d. -f1 | \
    sort > data_ids.txt
diff inv_ids.txt data_ids.txt
```

## plot_compare.py

Visualize and compare two GHCNM data files (raw vs adjusted, or any two versions).

**Key Features:**
- Two-subplot display: overlaid time series + difference plot
- Supports both input formats (with/without element code in column 12)
- Interactive display with ESC key to close, or save to file
- Statistics overlay (mean, std, max absolute difference)

### Supported Input Formats

The tool auto-detects these formats:

**Format A (Input files):** Whitespace-separated
```
PW100010148 1900 -9999 -9999 1703 1971 ...
```

**Format B (PHAMain output):** Fixed-width with element code
```
PW10001014831900 -9999    -9999     1703     1971    ...
```
- Column 12 contains single-digit element code:
  - 1 = TMAX
  - 2 = TMIN
  - 3 = TAVG
  - 5 = TDTR

### Usage

```bash
python3 plot_compare.py FILE1 FILE2 [OPTIONS]
```

**Options:**
- `-o, --output FILE`: Save plot to image file (PNG, PDF, etc.) instead of displaying
- `--label1 TEXT`: Label for first dataset (default: "Dataset 1")
- `--label2 TEXT`: Label for second dataset (default: "Dataset 2")
- `--by-year`: Display annual averages instead of monthly data (requires at least 6 months per year)
- `-v, --verbose`: Verbose output

### Examples

**Interactive comparison:**
```bash
python3 plot_compare.py \
    pw-data-v4/data/raw/PW100010148.raw.tavg \
    pw-data-v4/output/adjusted/tavg/PW100010148.WMs.pwv4.tavg
```

**Save to file with custom labels:**
```bash
python3 plot_compare.py \
    pw-data-v4/data/raw/PW100010148.raw.tavg \
    pw-data-v4/output/adjusted/tavg/PW100010148.WMs.pwv4.tavg \
    -o comparison.png \
    --label1 "Raw v52i input" \
    --label2 "v4 adjusted" \
    -v
```

**Compare two different PHA runs:**
```bash
python3 plot_compare.py \
    output1/USC00012345.WMs.v1.tmax \
    output2/USC00012345.WMs.v2.tmax \
    --label1 "Run 1 (confirm=2)" \
    --label2 "Run 2 (confirm=3)"
```

**Compare annual averages (removes seasonal variation):**
```bash
python3 plot_compare.py \
    pw-data-v4/data/raw/PW100010148.raw.tavg \
    pw-data-v4/output/adjusted/tavg/PW100010148.WMs.pwv4.tavg \
    --by-year \
    --label1 "Raw annual avg" \
    --label2 "Adjusted annual avg"
```

### Output

**Top subplot:** Overlaid time series showing both datasets
**Bottom subplot:** Difference (Dataset 1 - Dataset 2) with statistics box

Statistics displayed:
- Mean difference (°C)
- Standard deviation (°C)
- Maximum absolute difference (°C)

## plot_compare_average.py

Compare network averages from two directories of station data files.

**Key Features:**
- Computes average temperature across all stations in each directory
- Visualizes difference between network averages
- Optional station ID filtering with glob patterns
- Supports multiple averaging strategies (currently: equal-weight)
- Future: area-weighted gridding

### Averaging Strategies

**equal-weight (default):** Simple mean across all stations
- For each (year, month), average all stations with data for that time point
- All stations weighted equally regardless of location

**gridded (planned):** Area-weighted gridding
- Grid stations into geographic cells
- Weight by area coverage
- Reduces bias from station clustering

### Station Filtering

Filter by station ID using glob patterns:
- Pattern applies to **first dot-separated segment** of filename (the station ID)
- Examples:
  - `PW*` - Only Peter's World stations
  - `US*` - Only US stations
  - `USH*` - Only USHCN stations
  - `*0148` - Stations ending in 0148

### Usage

```bash
python3 plot_compare_average.py DIR1 DIR2 [OPTIONS]
```

**Options:**
- `-o, --output FILE`: Save plot to image file instead of displaying
- `--label1 TEXT`: Label for first network (default: "Directory 1")
- `--label2 TEXT`: Label for second network (default: "Directory 2")
- `--filter PATTERN`: Glob pattern to filter station IDs
- `--strategy {equal-weight}`: Averaging strategy (default: equal-weight)
- `--by-year`: Display annual averages instead of monthly data (requires at least 6 months per year)
- `-v, --verbose`: Verbose output

### Examples

**Compare raw vs adjusted network averages:**
```bash
python3 plot_compare_average.py \
    pw-data-v4/data/raw/ \
    pw-data-v4/output/adjusted/tavg/ \
    --label1 "Raw Network" \
    --label2 "Adjusted Network"
```

**Compare with station filter (only PW stations):**
```bash
python3 plot_compare_average.py \
    pw-data-v3/monthly/raw/ \
    pw-data-v4/output/adjusted/tavg/ \
    --filter "PW*" \
    --label1 "v52i raw" \
    --label2 "v4 adjusted" \
    -v
```

**Compare two different PHA parameter runs:**
```bash
python3 plot_compare_average.py \
    output-run1/adjusted/tmax/ \
    output-run2/adjusted/tmax/ \
    --label1 "confirm=2" \
    --label2 "confirm=3" \
    -o run_comparison.png
```

**Filter by US stations only:**
```bash
python3 plot_compare_average.py \
    data/raw/ \
    output/adjusted/tavg/ \
    --filter "US*" \
    --label1 "Raw (US only)" \
    --label2 "Adjusted (US only)"
```

**Compare annual network averages (cleaner trends):**
```bash
python3 plot_compare_average.py \
    pw-data-v4/data/raw/ \
    pw-data-v4/output/adjusted/tavg/ \
    --by-year \
    --label1 "Raw network annual" \
    --label2 "Adjusted network annual" \
    -v
```

### Output

**Top subplot:** Network average time series (both networks overlaid)
- Shows number of stations used in each network

**Bottom subplot:** Difference between network averages (Network 1 - Network 2)
- Statistics box showing:
  - Mean difference (°C)
  - Standard deviation (°C)
  - Maximum absolute difference (°C)
  - Averaging strategy used

### Use Cases

**Quality assessment:**
- Compare network average before and after PHA adjustment
- Check if adjustments introduce systematic bias
- Validate that network trends are preserved

**Parameter sensitivity:**
- Compare outputs with different `pha.confirm` values
- Test impact of `pha.snht-threshold` on network average
- Evaluate different neighbor selection strategies

**Version comparison:**
- Compare v52i vs v4 network averages
- Validate reconstruction against original code

**Regional analysis:**
- Use `--filter` to analyze specific regions
- Compare network averages by country or station network

## Known Issues

- Some benchmark data files may contain corrupted records (e.g., `PW1adata_fi.raw.tavg`). These malformed lines are skipped with warnings when using `-v` flag.
- Station names are truncated to 30 characters maximum in the v4 format.
- Timezone information from v3 format is discarded (not used in v4).

## Future Tools

Additional helpers planned:

- **compare_v52i_v4_output.py**: Compare PHA outputs between v52i and v4 runs
- **validate_formats.py**: Validate input data conforms to expected formats
- **extract_changepoints.py**: Extract changepoint information from WMs files
