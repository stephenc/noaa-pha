# GHCNM PHA Helper Tools

Python utilities for working with GHCNM v3/v52i and v4 data formats.

## Prerequisites

These tools use standard Python 3 libraries only. No external dependencies required.

If you want to use `uv` for project management:

```bash
cd helpers
uv sync
```

## convert_v3_to_v4.py

Converts GHCNM v3/v52i format data to GHCNM v4 format.

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

#### Convert Station List

Convert a v3 station list to v4 inventory format:

```bash
python3 convert_v3_to_v4.py station-list INPUT_FILE OUTPUT_FILE [-v]
```

Example:
```bash
python3 convert_v3_to_v4.py station-list \
    world1_stnlist.tavg \
    ghcnm.inv \
    -v
```

#### Convert Single Data File

Convert a v3 data file to v4 format:

```bash
python3 convert_v3_to_v4.py data-file INPUT_FILE OUTPUT_FILE [--flags "   "] [-v]
```

Example:
```bash
python3 convert_v3_to_v4.py data-file \
    PW100027370.raw.tavg \
    PW100027370.raw.tavg \
    -v
```

The `--flags` parameter specifies the 3-character flag string to add after each value. Default is three spaces (`"   "`).

#### Convert Entire Directory

Convert all data files in a directory:

```bash
python3 convert_v3_to_v4.py directory INPUT_DIR OUTPUT_DIR [--pattern "*.raw.*"] [--flags "   "] [-v]
```

Example:
```bash
python3 convert_v3_to_v4.py directory \
    world1/monthly/raw/ \
    world1_v4/data/ghcnm_v4/raw/ \
    --pattern "*.raw.tavg" \
    -v
```

### Complete Example: Converting Peter's World

Peter's World is a benchmark dataset from the v52i branch containing 7720 stations:

```bash
# 1. Extract the benchmark data (if not already done)
cd /tmp
git clone -b stable-v52i https://github.com/noaa/ghcn-v4-pairwise.git phav52i
cd phav52i/testdata/benchmark
tar xzf benchmark.tar.gz

# 2. Convert station list
cd world1/meta
python3 /path/to/convert_v3_to_v4.py station-list \
    world1_stnlist.tavg \
    world1.inv \
    -v

# 3. Convert all monthly data files
python3 /path/to/convert_v3_to_v4.py directory \
    ../monthly/raw/ \
    /tmp/world1_v4_converted/ \
    --pattern "*.raw.tavg" \
    -v
```

This converts:
- 7720 stations
- Approximately 437,000+ monthly records (depending on data availability)

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

## Known Issues

- Some benchmark data files may contain corrupted records (e.g., `PW1adata_fi.raw.tavg`). These malformed lines are skipped with warnings when using `-v` flag.
- Station names are truncated to 30 characters maximum in the v4 format.
- Timezone information from v3 format is discarded (not used in v4).

## Future Tools

Additional helpers planned:

- **compare_v52i_v4_output.py**: Compare PHA outputs between v52i and v4 runs
- **validate_formats.py**: Validate input data conforms to expected formats
- **extract_changepoints.py**: Extract changepoint information from WMs files
