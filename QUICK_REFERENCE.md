# GHCNM PHA Quick Reference

One-page cheat sheet for common operations.

## Build & Test

```bash
make              # Build all programs
make test         # Run tests
make clean        # Clean build artifacts
make help         # Show all targets
```

## Directory Setup

```bash
mkdir -p data/ghcnm_v4/raw
mkdir -p data/history
mkdir -p output/neighbors
mkdir -p output/ghcnm_v4_adjusted/{tmax,tmin,tavg}
```

## File Naming Conventions

| File Type | Pattern | Example |
|-----------|---------|---------|
| Inventory | `*.inv` | `ghcnm.inv` |
| Raw data | `{station}.raw.{elem}` | `USC00084731.raw.tmax` |
| Adjusted data | `{station}.WMs.{ver}.{elem}` | `USC00084731.WMs.v1.tmax` |
| History | `{station}.his` | `USC00084731.his` |
| Neighbors (dist) | `neighbors-distance.txt` | - |
| Neighbors (corr) | `neighbors-correlation.txt` | - |

## Essential Properties

### Minimal Configuration
```properties
pha.element = tmax
pha.path.station-metadata = data/ghcnm.inv
pha.path.station-element-data-in = data/ghcnm_v4/raw/
pha.path.station-element-data-out = output/adjusted/tmax/
```

### Most Important Settings
```properties
# Logging
pha.logger.filename = pha.log
pha.logger.level = INFO

# Core
pha.begin-year = 1880
pha.element = tmax|tmin|tavg
pha.input-data-type = raw

# Neighbors
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.final-neighbor-limit = 20
pha.neighbors.method = first-diffs|monthly-anomaly|distance-only
pha.neighbors.min-coefficient = 0.1

# Detection
pha.use-history-files = -1|0|1
pha.snht-threshold = 1|5|10
pha.confirm = 3
```

## Running Programs

### Main PHA Process
```bash
./bin/PHAMain                    # Reads ghcnm-pha.properties
```

### Pre-PHA Steps
```bash
# Quality control
./bin/ghcnm_qc <inv> <dat> <params>

# Clean (remove bad stations)
./bin/ghcnm_clean <in_inv> <in_dat> <threshold> \
    <remove_list> <asterisk_list> <keep_list> \
    <out_inv> <out_dat>

# Apply corrections
./bin/ghcnm_edit <in_inv> <in_dat> <meta_edits> \
    <data_edits> <out_inv> <out_dat>
```

### Helper Programs
```bash
# Calculate station distances
./bin/ushcn_dist.v6.combo -m <meta> -o <out> -u <ncand>

# Calculate correlations
./bin/ushcn_corr.v5a.combo <end_year> <elem_code> \
    -d <dist_file> -m <meta> -C <cand_dir> -N <net_dir> \
    -o <out> -p <proc> -u <ncand>

# Generate TAVG from TMAX/TMIN
./bin/gen_avg.3flg -v byear=1880 -v eyear=2024 \
    -v ddir=data/ghcnm_v4 -v vproc=raw station_list.txt
```

## Data Format Quick Reference

### Inventory (.inv)
```
Cols 1-11:   Station ID (A11)
Cols 13-20:  Latitude (F8.4)
Cols 22-30:  Longitude (F9.4)
Cols 32-37:  Elevation (F6.1)
Cols 39-68:  Station Name (A30)
```

### Monthly Data (.raw.tmax, etc.)
```
Cols 1-11:   Station ID (A11)
Cols 13-16:  Year (I4)
Cols 17-124: 12 months × (value I6 + flags A3)
```
- Value: hundredths of °C, -9999 = missing
- Flags: DM QC DS (3 chars)

### Properties File
```properties
key = value                    # Basic
key = {other_key}/path        # Substitution
key = value-{arg:flag}        # Command-line arg
```

## Common AWK Operations

### Extract Station IDs
```bash
cut -c1-11 ghcnm.inv
```

### Filter by Region
```bash
awk '$2 >= 30 && $2 <= 40 && $3 >= -120 && $3 <= -100' ghcnm.inv
```

### Count Data Years
```bash
wc -l USC00084731.raw.tmax
```

### Check for Missing Data
```bash
grep -c -- "-9999" USC00084731.raw.tmax
```

## Tuning for Performance vs Quality

### Fast (Testing)
```properties
pha.neighbors.distance-neighbor-limit = 10
pha.neighbors.final-neighbor-limit = 5
pha.neighbors.method = distance-only
pha.confirm = 1
pha.adjust.min-neighbors = 3
```

### Balanced (Default)
```properties
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.final-neighbor-limit = 20
pha.neighbors.method = first-diffs
pha.confirm = 3
pha.adjust.min-neighbors = 5
```

### High Quality (Slow)
```properties
pha.neighbors.distance-neighbor-limit = 60
pha.neighbors.final-neighbor-limit = 30
pha.neighbors.method = first-diffs
pha.neighbors.min-coefficient = 0.2
pha.confirm = 4
pha.adjust.min-neighbors = 7
```

## Troubleshooting Commands

### Check Log for Errors
```bash
grep -i error pha.log
grep -i warning pha.log
tail -50 pha.log
```

### Verify Output Created
```bash
ls -lh output/ghcnm_v4_adjusted/tmax/ | wc -l
```

### Check Adjustment Counts
```bash
# Count 'A' flags in output (adjustments applied)
grep -o A output/adjusted/tmax/USC00084731.WMs.v1.tmax | wc -l
```

### Validate Data Format
```bash
# Check line length (should be 124 chars)
awk '{print length}' data/ghcnm_v4/raw/USC00084731.raw.tmax | sort -u

# Check station ID format (should be 11 chars)
cut -c1-11 data/ghcnm.inv | awk '{print length}' | sort -u
```

## Environment Variables for Legacy Programs

Some older Fortran programs use environment variables:

```bash
export NEIGH_CLOSE=40      # Initial neighbor count
export NEIGH_FINAL=20      # Final neighbor count
export NEIGH_CORR=1diff    # Correlation method
export CORR_LIM=0.1        # Min correlation
export MIN_STNS=7          # Min stations for coverage

./bin/ushcn_dist.v6.combo -m meta.txt -o dist.txt
./bin/ushcn_corr.v5a.combo 2020 1 -d dist.txt ...
```

## File Size Estimates

| Item | Small Dataset | Regional | Full GHCNM |
|------|--------------|----------|------------|
| Stations | 10-50 | 100-1000 | ~27,000 |
| Inventory | <10 KB | 50-500 KB | ~2 MB |
| Data/element | <100 KB | 1-10 MB | ~100 MB |
| Neighbor files | <50 KB | 500 KB - 5 MB | ~50 MB |
| Output | ~same as input | ~same | ~same |
| Process time | seconds | minutes | hours-days |

## Element Codes

| Code | Description | Units |
|------|-------------|-------|
| `tmax` | Maximum temperature | 0.01°C |
| `tmin` | Minimum temperature | 0.01°C |
| `tavg` | Mean temperature | 0.01°C |
| `tdtr` | Diurnal temperature range | 0.01°C |
| `prcp` | Precipitation | 0.1mm |

## Flag Meanings

### DM Flag (Data Measurement)
- (blank) = Standard
- `E` = Estimated/filled
- Other = Special observation types

### QC Flag (Quality Control)
- (blank) = Passed
- `X` = Failed, removed
- `Q` = Questionable
- `D` = Duplicate
- `R` = Record extreme
- `K` = Streak
- `S`/`T` = Spatial outlier
- `I` = Internal inconsistency

### DS Flag (Data Source)
- (blank) = Standard
- Various codes for different sources

## Quick Diagnosis

| Symptom | Likely Cause | Fix |
|---------|--------------|-----|
| No output files | Wrong output path | Check `pha.path.station-element-data-out` |
| No neighbors found | Isolated station | Reduce `min-coefficient` to 0.0 |
| No adjustments | `confirm` too high | Reduce to 1 or 2 |
| Too many adjustments | `snht-threshold` too high | Use 1 or 5 instead of 10 |
| Crashes on start | Bad properties | Check syntax, required keys |
| Slow processing | Too many stations/neighbors | Reduce `distance-neighbor-limit` |

## Getting Help

1. **Check the guides**:
   - [GETTING_STARTED.md](GETTING_STARTED.md)
   - [WORKFLOW.md](WORKFLOW.md)
   - [examples/DATA_FORMAT_EXAMPLES.md](examples/DATA_FORMAT_EXAMPLES.md)

2. **Check the log file**: `pha.log` or `pha-{element}.log`

3. **Run unit tests**: `make test` to verify build

4. **Check file formats**: See DATA_FORMAT_EXAMPLES.md

5. **Validate data**: Use example scripts in `examples/`

## Useful One-Liners

```bash
# Count stations by country code (first 2 chars)
cut -c1-2 ghcnm.inv | sort | uniq -c

# Find stations with < 50 years data
for f in data/ghcnm_v4/raw/*.raw.tmax; do
    [ $(wc -l < "$f") -lt 50 ] && basename "$f"
done

# Calculate average adjustment magnitude
grep -o 'A' output/adjusted/tmax/*.WMs.v1.tmax | \
    wc -l | awk '{print $1/NR " adjustments/station"}'

# Find all properties files
find . -name "*.properties"

# Test if neighbor files exist
[ -f output/neighbors-correlation.txt ] && echo "Found" || echo "Missing"
```
