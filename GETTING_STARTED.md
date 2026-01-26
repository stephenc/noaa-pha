# Getting Started with GHCNM v4 PHA

This guide will help you run the Pairwise Homogeneity Adjustment (PHA) analysis on climate station data.

## Quick Start

### 1. Build the Software

```bash
make
make test  # Optional but recommended
```

The build creates executables in `./bin/` and the tests verify the core functionality works.

### 2. Obtain Input Data

To run the PHA analysis, you need:

**Required:**
- **Station Inventory** (`ghcnm.inv`): Metadata for all stations (ID, lat/lon, elevation, name)
- **Monthly Temperature Data**: Raw temperature data files in GHCNM 3-flag format
  - One file per station, named like: `{station_id}.raw.{element}`
  - Elements: `tmax` (max temp), `tmin` (min temp), or `tavg` (mean temp)
  - Values in hundredths of degrees Celsius

**Optional:**
- **Station History Files** (`.his`): Documented station moves/changes
- **Neighbor Files**: Pre-computed neighbor lists (otherwise calculated automatically)

#### Where to Get Data

The official GHCN-Monthly v4 dataset is available from NOAA:
```
ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/
```

Key files:
- `ghcnm.tavg.latest.qcu.tar.gz` - Mean temperature
- `ghcnm.tmax.latest.qcu.tar.gz` - Maximum temperature
- `ghcnm.tmin.latest.qcu.tar.gz` - Minimum temperature

These contain the inventory and pre-processed data you can use as input.

### 3. Set Up Directory Structure

Create the following directories:

```bash
mkdir -p data/ghcnm_v4/raw
mkdir -p data/history
mkdir -p output/neighbors
mkdir -p output/ghcnm_v4_adjusted/{tmax,tmin,tavg}
```

Place your data:
- `data/ghcnm.inv` - Station inventory file
- `data/ghcnm_v4/raw/*.raw.{element}` - Station data files
- `data/history/*.his` - Station history files (optional)

### 4. Create a Configuration File

Copy and customize the test properties file:

```bash
cp build/ghcnm-pha.test.properties ghcnm-pha.properties
```

Edit `ghcnm-pha.properties` and adjust these key settings:

```properties
# === Core Parameters ===
pha.begin-year = 1880
pha.element = tmax
pha.input-data-type = raw
pha.version = v1

# === File Paths ===
pha.path.station-metadata = data/ghcnm.inv
pha.path.neighbors.distance = output/neighbors/neighbors-distance.txt
pha.path.neighbors.correlation = output/neighbors/neighbors-correlation.txt
pha.path.station-element-data-in = data/ghcnm_v4/{pha.input-data-type}/
pha.path.station-element-data-out = output/ghcnm_v4_adjusted/{pha.element}/
pha.path.station-history = data/history/

# === Neighbor Selection ===
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.method = first-diffs
pha.neighbors.min-coefficient = 0.1
pha.neighbors.final-neighbor-limit = 20

# === Changepoint Detection ===
pha.use-history-files = 1
pha.snht-threshold = 5
pha.confirm = 3
```

### 5. Run the Analysis

```bash
./bin/PHAMain
```

The program will:
1. Read station metadata and data
2. Calculate distances between stations
3. Compute correlations to select neighbors
4. Detect changepoints (breaks) in station records
5. Calculate adjustments for inhomogeneities
6. Write adjusted data to the output directory

Output files will be in `output/ghcnm_v4_adjusted/{element}/`

## Understanding the Workflow

### Complete PHA Pipeline

The full GHCNM production pipeline has multiple stages. Here's the simplified workflow:

```
┌─────────────────────────────────────────────────────────┐
│ 1. DATA PREPARATION (Manual/External)                  │
├─────────────────────────────────────────────────────────┤
│ • Obtain raw station data from various sources          │
│ • Convert to GHCNM 3-flag format                       │
│ • Create inventory file                                 │
│ • (Optional) Create station history files               │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ 2. QUALITY CONTROL (Optional but recommended)          │
├─────────────────────────────────────────────────────────┤
│ ./bin/ghcnm_qc <inventory> <datafile> <params>         │
│ • Checks for duplicate years, record extremes          │
│ • Spatial consistency checks                            │
│ • Flags suspect data points                            │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ 3. HOMOGENEITY ADJUSTMENT (PHAMain)                    │
├─────────────────────────────────────────────────────────┤
│ ./bin/PHAMain                                           │
│ • Reads config from properties file                     │
│ • Identifies neighboring stations                       │
│ • Detects changepoints (breakpoints)                   │
│ • Calculates adjustments                                │
│ • Writes adjusted series                                │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ 4. POST-PROCESSING (Optional)                          │
├─────────────────────────────────────────────────────────┤
│ • Calculate averages/anomalies                          │
│ • Generate derived products (tavg from tmax/tmin)      │
│ • Format for distribution                               │
└─────────────────────────────────────────────────────────┘
```

### What PHAMain Does

The PHA (Pairwise Homogeneity Adjustment) algorithm:

1. **Neighbor Selection**: For each target station, finds nearby stations with similar climate patterns
2. **Pairwise Difference Series**: Creates difference series between target and each neighbor
3. **Changepoint Detection**: Uses SNHT (Standard Normal Homogeneity Test) to find breaks
4. **Attribution**: Determines which station (target or neighbor) caused each break
5. **Adjustment Calculation**: Estimates magnitude of inhomogeneity at each break
6. **Series Adjustment**: Applies adjustments to create homogeneous record

## Configuration Reference

### Key Properties Explained

**For complete details on every property, see [PROPERTIES_REFERENCE.md](PROPERTIES_REFERENCE.md)**

#### Neighbor Selection
- `pha.neighbors.distance-neighbor-limit` (default: 40)
  - How many closest stations to initially consider

- `pha.neighbors.method` (options: `first-diffs`, `monthly-anomaly`, `distance-only`)
  - `first-diffs`: Use first differences for correlation (recommended for temperature)
  - `monthly-anomaly`: Use anomalies from monthly climatology
  - `distance-only`: Skip correlation, use closest by distance

- `pha.neighbors.min-coefficient` (default: 0.1)
  - Minimum correlation to include a neighbor

- `pha.neighbors.final-neighbor-limit` (default: 20)
  - Maximum neighbors to use in final analysis

#### Changepoint Detection
- `pha.use-history-files` (options: -1, 0, 1)
  - `-1`: Only use documented metadata from .his files
  - `0`: Ignore history, detect statistically only
  - `1`: Use history + statistical detection (recommended)

- `pha.snht-threshold` (options: 1, 5, 10)
  - Significance level for SNHT test (percent)
  - Lower = more sensitive (more breaks detected)

- `pha.confirm` (default: 3)
  - Minimum number of neighbor pairs that must agree on a break
  - Higher = more conservative

- `pha.bic-penalty` (options: `bic`, `aic`, `none`)
  - Model selection criterion (BIC recommended)

#### Adjustment Parameters
- `pha.adjust.min-length` (default: 18)
  - Minimum segment length (months) to calculate adjustment

- `pha.adjust.min-neighbors` (default: 5)
  - Minimum neighbors needed to estimate adjustment

- `pha.adjust.est-method` (options: `med`, `avg`, `qav`)
  - `med`: Median of pairwise estimates (most robust)
  - `avg`: Mean of estimates
  - `qav`: Quasi-average (trimmed mean)

## Working with Small Datasets

For testing or small regional analyses:

1. **Select a subset of stations** from the inventory:
   ```bash
   # Extract stations in a region (e.g., latitude 30-40, longitude -120 to -110)
   awk '$2 >= 30.0 && $2 <= 40.0 && $3 >= -120.0 && $3 <= -110.0' \
       data/ghcnm.inv > data/regional.inv
   ```

2. **Extract corresponding data files** (you'll need to identify and copy the station files)

3. **Use the regional inventory**:
   ```properties
   pha.path.station-metadata = data/regional.inv
   ```

4. **Reduce neighbor requirements**:
   ```properties
   pha.neighbors.distance-neighbor-limit = 10
   pha.neighbors.final-neighbor-limit = 5
   pha.neighbors.min-coefficient = 0.0
   pha.confirm = 1
   ```

## Troubleshooting

### "Error reading station data file"
- Check file paths in properties file
- Verify data files exist and match naming convention
- Ensure data format is correct (see README.md "Data Formats" section)

### "Insufficient neighbors for station"
- Reduce `pha.neighbors.min-coefficient`
- Reduce `pha.confirm` requirement
- Increase `pha.neighbors.distance-neighbor-limit`
- Check if station is isolated (may need larger network)

### Test failures on build
The unit test suite includes one known floating-point precision issue that may fail on some platforms. As long as only 1 test fails with a correlation value mismatch, the software is working correctly.

### Out of memory errors
- Process smaller regions separately
- Reduce `pha.neighbors.distance-neighbor-limit`
- Increase system memory/swap

## Example: Processing a Single Element

Complete example for maximum temperature:

```bash
# 1. Build
make clean
make

# 2. Prepare data directory structure
mkdir -p data/ghcnm_v4/raw
mkdir -p output/ghcnm_v4_adjusted/tmax

# 3. Download and extract GHCNM data
cd data
wget ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tmax.latest.qcu.tar.gz
tar -xzf ghcnm.tmax.latest.qcu.tar.gz
mv ghcnm.tmax.v4.0.1.* ghcnm.inv
# Extract individual station files to data/ghcnm_v4/raw/
# (This step requires parsing the .dat file - see README for format)

# 4. Create config
cat > ghcnm-pha.properties << 'EOF'
pha.logger.filename = pha-tmax.log
pha.logger.level = INFO
pha.begin-year = 1880
pha.element = tmax
pha.input-data-type = raw
pha.version = v1
pha.path.station-metadata = data/ghcnm.inv
pha.path.neighbors.distance = output/neighbors-distance.txt
pha.path.neighbors.correlation = output/neighbors-correlation.txt
pha.path.station-element-data-in = data/ghcnm_v4/raw/
pha.path.station-element-data-out = output/ghcnm_v4_adjusted/tmax/
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.method = first-diffs
pha.neighbors.final-neighbor-limit = 20
pha.use-history-files = 0
pha.snht-threshold = 5
pha.confirm = 3
EOF

# 5. Run
cd ..
./bin/PHAMain

# 6. Check results
ls -lh output/ghcnm_v4_adjusted/tmax/
cat pha-tmax.log
```

## Additional Resources

- **[WORKFLOW.md](WORKFLOW.md)** - Visual diagrams of the complete processing pipeline and data flow
- **[PROPERTIES_REFERENCE.md](PROPERTIES_REFERENCE.md)** - Canonical reference for all configuration properties
- **[examples/DATA_FORMAT_EXAMPLES.md](examples/DATA_FORMAT_EXAMPLES.md)** - Detailed file format examples with parsing code
- **[examples/prepare_sample_data.sh](examples/prepare_sample_data.sh)** - Automated script to download and prepare sample data
- **[README.md](README.md)** - Complete technical reference for all programs

## Next Steps

- Read the full [README.md](README.md) for detailed program descriptions and file format specifications
- Try the [examples/prepare_sample_data.sh](examples/prepare_sample_data.sh) script to automatically download sample data
- Examine the properties files in `build/` for more configuration examples
- Review log files after runs to understand what the algorithm is doing
- Compare your results with official GHCNM v4 products to validate

## Getting Help

This is a reconstructed version of NOAA's internal production code.

- For issues with this reconstruction: Check the GitHub repository issues
- For questions about the PHA methodology: See the GHCNM v4 technical documentation
- For official GHCNM data: https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-monthly

## Additional Utilities

The repository includes many helper programs for data preparation and analysis:

- `ghcnm_qc`: Quality control checks
- `ghcnm_clean`: Remove stations with insufficient data
- `ghcnm_edit`: Apply manual corrections
- `ushcn_dist.v6.combo`: Calculate station distances
- `ushcn_corr.v5a.combo`: Calculate correlations
- `gen_avg.3flg`: Generate TAVG from TMAX/TMIN

See README.md for detailed usage of each program.
