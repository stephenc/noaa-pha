# GHCNM v52i to v4 Property Mapping

**Comparison of PHA v52i environment variables to GHCNM v4 properties**

This document maps the algorithm parameters from the well-documented v52i version (GHCNM v3 era) to the reconstructed v4 properties, helping users configure v4 to produce results similar to v52i.

---

## Executive Summary

To produce results similar to v52i with the v4 code, use these property values:

```properties
# Neighbor Selection (v52i standard configuration)
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.method = first-diffs
pha.neighbors.min-coefficient = 0.1
pha.neighbors.min-station-coverage = 7
pha.neighbors.final-neighbor-limit = 20

# Changepoint Detection (v52i defaults)
pha.use-history-files = 1
pha.snht-threshold = 5
pha.bic-penalty = bic
pha.amploc-percent = 92
pha.confirm = 2

# Adjustment Calculation (v52i defaults)
pha.adjust.min-length = 18
pha.adjust.min-neighbors = 2
pha.adjust.remove-outliers = true
pha.adjust.window = 0
pha.adjust.filter-method = conf
pha.adjust.est-method = med
pha.remove-insignificant = true
```

---

## Detailed Mapping

### Source Files Referenced

- **v52i:** `inhomog.combo.mthly.incl`, `inhomog.parm.system.MLY.incl`
- **v4:** `PropertyParameters.f95`, `AlgorithmParameters.f95`

---

## Property Comparison Table

| v52i Environment Variable | v52i Typical Value | v4 Property | v4 Equivalent Value | Notes |
|--------------------------|-------------------|-------------|---------------------|-------|
| **NEIGHBOR SELECTION** |||||
| `NEIGH_CLOSE` | `40` | `pha.neighbors.distance-neighbor-limit` | `40` | Number of closest stations to consider |
| `NEIGH_FINAL` | `20` | `pha.neighbors.final-neighbor-limit` | `20` | Final number of neighbors after correlation |
| `NEIGH_CORR` | `1diff` | `pha.neighbors.method` | `first-diffs` | Correlation method (1diff = first differences) |
| `MIN_STNS` | `7` | `pha.neighbors.min-station-coverage` | `7` | Minimum neighbors with data per time period |
| (implicit min correlation) | `0.1` | `pha.neighbors.min-coefficient` | `0.1` | Minimum correlation threshold |
| **CHANGEPOINT DETECTION** |||||
| `SNHT_THRES` | `5` | `pha.snht-threshold` | `5` | SNHT significance: 1=97.5%, 5=95%, 10=90% |
| `BIC_PENALTY` | `bic` | `pha.bic-penalty` | `bic` | Model selection criterion |
| `CONFIRM` | `2` | `pha.confirm` | `2` | Minimum neighbor pairs to confirm break |
| `SHF_META` | `1` | `pha.use-history-files` | `1` | Use station history metadata: -1/0/1 |
| `AMPLOC_PCT` | `92` (92.5%) | `pha.amploc-percent` | `92` | Timing uncertainty confidence limit |
| **ADJUSTMENT CALCULATION** |||||
| `ADJ_MINLEN` | `18` | `pha.adjust.min-length` | `18` | Minimum segment length (months) |
| `ADJ_MINPAIR` | `2` | `pha.adjust.min-neighbors` | `2` | Minimum station pairs for adjustment |
| `ADJ_OUTLIER` | `1` | `pha.adjust.remove-outliers` | `true` | Remove outlier estimates |
| `ADJ_WINDOW` | `0` | `pha.adjust.window` | `0` | Months to average (0=no limit) |
| `ADJ_FILTER` | `conf` | `pha.adjust.filter-method` | `conf` | Outlier filtering method |
| `ADJ_EST` | `med` | `pha.adjust.est-method` | `med` | Adjustment estimation method |
| `NS_LOOP` | `1` | `pha.remove-insignificant` | `true` | Remove non-significant breakpoints |
| **SYSTEM PARAMETERS** |||||
| `maxnstns` | `200` | (implicit in final-neighbor-limit) | - | Maximum neighbors (v4 allocates dynamically) |
| `begyr` | `1895` | `pha.begin-year` | `1880` | Beginning year (v4 typically uses 1880) |
| `endyr` | `2015` | (dynamic) | current year | End year (v4 uses current year) |
| `minser` | `5` | `MIN_MONTHS_RAW` | `5` | Minimum months in raw series (hardcoded in v4) |
| `minann` | `5` | `MIN_YEARS_FOR_MONTHLY_AVE` | `5` | Minimum years for monthly average (hardcoded) |
| `minlenshf` | `24` | (implicit) | - | Minimum station history segment length |
| `ninh` | `80` | `MAX_CHANGEPOINTS` | `80` | Maximum changepoints per station (hardcoded) |
| `amiss` | `-99.99` | `MISSING_REAL` | `-99.99` | Missing data value (hardcoded) |

---

## Algorithm Parameter Details

### Neighbor Selection

#### v52i Implementation
In v52i, neighbors are selected via environment variables passed to `ushcn_dist.v5a.combo.f` and `ushcn_corr.v5a.combo.f`:

```fortran
c From inhomog.combo.mthly.incl:
integer ndist           ! NEIGH_CLOSE - closest neighbors to consider
integer nsnet           ! NEIGH_FINAL - final number after correlation
character*5 corr_type   ! NEIGH_CORR - correlation method (1diff, anom)
integer minstns         ! MIN_STNS - minimum coverage per period
```

**v52i Typical Settings:**
- `NEIGH_CLOSE=40` - Examines 40 closest stations
- `NEIGH_CORR=1diff` - Uses first differences (removes trends)
- `MIN_STNS=7` - Requires 7 stations with data
- `NEIGH_FINAL=20` - Keeps best 20 after correlation

#### v4 Equivalent
```properties
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.method = first-diffs
pha.neighbors.min-station-coverage = 7
pha.neighbors.final-neighbor-limit = 20
pha.neighbors.min-coefficient = 0.1
```

**Key Difference:**
- v52i: Minimum correlation is hardcoded in correlation routine
- v4: Minimum correlation is configurable property (`pha.neighbors.min-coefficient`)

---

### Changepoint Detection

#### v52i Implementation
From `inhomog.parm.system.MLY.incl` and `inhomog.combo.mthly.incl`:

```fortran
c SNHT significance level (SNHT_THRES)
c 1 = 97.5%; 5 = 95%; 10=90%
integer isnht

c Penalty function for Bayesian (BIC_PENALTY)
c bic, aic, none
character*4 bic_type

c Number indicating the hits in station pairs to confirm a chgpt (CONFIRM)
c <2>,3,4,5
integer iconfirm

c Toggle to use or not use Station History metadata (SHF_META)
c 0 = do NOT use, 1 = use
integer ishfmeta

c Index for the "amp vs loc" percent inclusion (AMPLOC_PCT)
c 1 (90%), <2> (92.5%), 3 (95%)
integer mypct
```

**v52i Standard Configuration:**
```bash
export SNHT_THRES=5       # 95% significance
export BIC_PENALTY=bic    # Bayesian Information Criterion
export CONFIRM=2          # At least 2 neighbor pairs must agree
export SHF_META=1         # Use station history files
export AMPLOC_PCT=92      # 92.5% confidence for timing uncertainty
```

**v52i Algorithm Behavior:**
- `CONFIRM=2` uses threshold table: `nhthres(nhss) = / 2, 2, 2, ... /`
  - This means 2 hits required regardless of network size
  - More liberal than v3's original confirm=3 setting
- `AMPLOC_PCT=92` gives merge radii from:
  ```fortran
  integer mrgyr(nrange, 3) = /
       29,  12,   7,  5,  3,  2,  1,   ! 90%
       36,  18,  12,  8,  6,  5,  5,   ! 92.5%
       59,  23,  12,  8,  6,  5,  5/   ! 95%
  ```
  - Larger amplitudes → smaller merge window (months)
  - 92.5% is middle ground between conservative and liberal

#### v4 Equivalent
```properties
pha.snht-threshold = 5
pha.bic-penalty = bic
pha.confirm = 2
pha.use-history-files = 1
pha.amploc-percent = 92
```

**Exact Match:**
The v4 code copied these arrays and logic directly from v52i:
- `MERGE_RADII` array in `AlgorithmParameters.f95` matches `mrgyr` exactly
- Amplitude ranges `AMP_RANGES` match `arange` exactly
- Same get_amploc_percent_index() conversion function

---

### Adjustment Calculation

#### v52i Implementation
From `inhomog.combo.mthly.incl`:

```fortran
c Adjustment estimate filter method (ADJ_FILTER)
c BICF = bic sig test per pair
c CONF = collective 95% conf interval of adj est dist
c BOTH = both BICF and CONF
c NONE = neither BICF or CONF
character*4 adjfilt

c Adjustment estimate method (ADJ_EST)
c MED = median value
c QAV = average of 25% & 75% values
c AVG = average
character*3 adjest

c Toggle to remove outliers (ADJ_OUTLIER)
c 0 = do NOT remove outliers, 1 = remove outliers
integer iadjout

c Number of months to average for adjustment estimate (ADJ_WINDOW)
c valid values = <0>, 24, 60, 120
integer iadjwin

c Minimum number of data values in segment (ADJ_MINLEN)
c valid values = <18>, 24, 36, 48
integer minlen

c Minimum number of station pairs to estimate adjustment (ADJ_MINPAIR)
c valid values = <2>, 3, 4, 5
integer minsta

c Toggle to remove non-significant breakpoints (NS_LOOP)
c 0 = do NOT remove NS breakpoints, 1 = remove NS breakpoints
integer insloop
```

**v52i Standard Configuration:**
```bash
export ADJ_EST=med        # Use median of pairwise estimates
export ADJ_FILTER=conf    # Tukey confidence interval filtering
export ADJ_OUTLIER=1      # Remove outliers before estimation
export ADJ_WINDOW=0       # Use all available data (no window limit)
export ADJ_MINLEN=18      # Minimum 18 months per segment
export ADJ_MINPAIR=2      # Minimum 2 station pairs needed
export NS_LOOP=1          # Remove non-significant breakpoints
```

**v52i Algorithm Notes:**
- `ADJ_EST=med` (median) is most robust to outliers
- `ADJ_FILTER=conf` uses Tukey method (IQR-based outlier detection)
- `ADJ_MINPAIR=2` is liberal - allows adjustment with few neighbors
- `NS_LOOP=1` iteratively removes statistically insignificant breaks

#### v4 Equivalent
```properties
pha.adjust.est-method = med
pha.adjust.filter-method = conf
pha.adjust.remove-outliers = true
pha.adjust.window = 0
pha.adjust.min-length = 18
pha.adjust.min-neighbors = 2
pha.remove-insignificant = true
```

**Naming Differences:**
- v52i: `ADJ_MINPAIR` → v4: `pha.adjust.min-neighbors`
- v52i: `NS_LOOP` → v4: `pha.remove-insignificant`
- v52i: uses integers 0/1 → v4: uses logical true/false

---

## Key Differences Between v52i and v4

### 1. Confirm Threshold Evolution

**v52i (GHCNM v3):**
- `CONFIRM=2` was the standard
- Threshold table: all values set to 2
- More liberal - easier to confirm breaks

**v4 (GHCNM v4):**
- Documentation suggests `CONFIRM=3` as typical
- More conservative than v52i
- Reduces false positive detections

**Recommendation:**
- Use `pha.confirm = 2` to match v52i behavior
- Use `pha.confirm = 3` for v4 operational standard

### 2. Neighbor Count Handling

**v52i:**
- Fixed maximum: `maxnstns = 200`
- Compiled into executable
- Buffer added: `nstns = nsnet + nsnet/2`

**v4:**
- Dynamic allocation based on properties
- No hardcoded maximum
- More flexible for large networks

### 3. Property vs Environment Variables

**v52i:**
- Uses environment variables exclusively
- Must be set before running
- No default values (program stops if undefined)

**v4:**
- Uses properties file
- All properties must be defined in file
- Supports property substitution `{pha.element}`
- More maintainable configuration

### 4. Input Data Handling

**v52i:**
- Separate candidate and network data directories
- Command line: `-C $candata -N $netdata`
- Distinction between target and neighbor stations

**v4:**
- Unified data directory approach
- All stations treated equally
- Simpler configuration

---

## Migration Guide: v52i to v4

### Step 1: Convert Environment Variables to Properties

If you have a v52i run script with environment variables:

```bash
# v52i environment variables
export NEIGH_CLOSE=40
export NEIGH_FINAL=20
export NEIGH_CORR=1diff
export MIN_STNS=7
export SNHT_THRES=5
export BIC_PENALTY=bic
export CONFIRM=2
export SHF_META=1
export ADJ_EST=med
export ADJ_FILTER=conf
export ADJ_OUTLIER=1
export ADJ_WINDOW=0
export ADJ_MINLEN=18
export ADJ_MINPAIR=2
export AMPLOC_PCT=92
export NS_LOOP=1
```

Convert to v4 properties file:

```properties
# v4 properties (v52i-equivalent configuration)

# Neighbor Selection
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.final-neighbor-limit = 20
pha.neighbors.method = first-diffs
pha.neighbors.min-station-coverage = 7
pha.neighbors.min-coefficient = 0.1

# Changepoint Detection
pha.snht-threshold = 5
pha.bic-penalty = bic
pha.confirm = 2
pha.use-history-files = 1
pha.amploc-percent = 92

# Adjustment Calculation
pha.adjust.est-method = med
pha.adjust.filter-method = conf
pha.adjust.remove-outliers = true
pha.adjust.window = 0
pha.adjust.min-length = 18
pha.adjust.min-neighbors = 2
pha.remove-insignificant = true
```

### Step 2: Add Required v4-Only Properties

v4 requires additional properties not present in v52i:

```properties
# General (required by v4)
pha.version = v52i-equiv
pha.begin-year = 1880
pha.element = tmax
pha.input-data-type = raw
pha.neighbors.input-data-type = raw

# File Paths
pha.path.station-metadata = data/ghcnm.inv
pha.path.neighbors.distance = output/neighbors-distance.txt
pha.path.neighbors.correlation = output/neighbors-correlation.txt
pha.path.neighbors.correlation-in = output/neighbors-correlation.txt
pha.path.station-element-data-in = data/ghcnm_v4/raw/
pha.path.neighbors.station-element-data-in = data/ghcnm_v4/raw/
pha.path.station-element-data-out = output/adjusted/tmax/
pha.path.station-history = data/history/

# Logger
pha.logger.filename = pha-v52i-equiv.log
pha.logger.level = INFO
pha.logger.print-to-stdout = true
pha.logger.append-datestamp = false
pha.logger.rollover-datestamp = false
```

### Step 3: Verify Data Format Compatibility

**v52i uses:**
- GHCN-M format with 3 flags per value
- Station ID (11 chars), Year (4 digits), 12 months × (value + flags)

**v4 expects:**
- Same format!
- File naming: `{station_id}.{data_type}.{element}`

If migrating from v52i, your data files should work directly.

### Step 4: Run Comparison Test

To verify v4 produces similar results to v52i:

1. Run v52i on test dataset
2. Run v4 with v52i-equivalent configuration
3. Compare:
   - Number of changepoints detected per station
   - Timing of changepoints (should be within merge window)
   - Magnitude of adjustments (should be similar within ~0.1°C)
   - Overall network statistics

Expected differences:
- Minor differences due to floating-point precision
- Small differences in correlation values
- Possible differences in edge cases

---

## Complete v52i-Equivalent Configuration

**File: `ghcnm-pha-v52i-equivalent.properties`**

```properties
# ====================================================================
# GHCNM PHA v52i-Equivalent Configuration
#
# This configuration reproduces v52i (GHCNM v3) algorithm behavior
# using the reconstructed GHCNM v4 codebase.
# ====================================================================

# === General Run Parameters ===
pha.version = v52i-equiv
pha.begin-year = 1880
pha.element = tmax
pha.input-data-type = raw
pha.neighbors.input-data-type = raw

# === File Path Configuration ===
pha.path.station-metadata = data/ghcnm.inv
pha.path.neighbors.distance = output/neighbors/neighbors-distance.txt
pha.path.neighbors.correlation = output/neighbors/neighbors-correlation.txt
pha.path.neighbors.correlation-in = output/neighbors/neighbors-correlation.txt
pha.path.station-element-data-in = data/ghcnm_v4/{pha.input-data-type}/
pha.path.neighbors.station-element-data-in = data/ghcnm_v4/{pha.neighbors.input-data-type}/
pha.path.station-element-data-out = output/ghcnm_v4_adjusted/{pha.element}/
pha.path.station-history = data/history/

# === Neighbor Selection Parameters (v52i standard) ===
# NEIGH_CLOSE = 40
pha.neighbors.distance-neighbor-limit = 40

# NEIGH_FINAL = 20
pha.neighbors.final-neighbor-limit = 20

# NEIGH_CORR = 1diff
pha.neighbors.method = first-diffs

# MIN_STNS = 7
pha.neighbors.min-station-coverage = 7

# Minimum correlation (not explicit in v52i, typically 0.1)
pha.neighbors.min-coefficient = 0.1

# === Changepoint Detection Parameters (v52i standard) ===
# SNHT_THRES = 5 (95% significance)
pha.snht-threshold = 5

# BIC_PENALTY = bic
pha.bic-penalty = bic

# AMPLOC_PCT = 92 (92.5% confidence)
pha.amploc-percent = 92

# CONFIRM = 2 (v52i standard, more liberal than v3's 3)
pha.confirm = 2

# === Adjustment Calculation Parameters (v52i standard) ===
# ADJ_MINLEN = 18
pha.adjust.min-length = 18

# ADJ_MINPAIR = 2
pha.adjust.min-neighbors = 2

# ADJ_OUTLIER = 1
pha.adjust.remove-outliers = true

# ADJ_WINDOW = 0
pha.adjust.window = 0

# ADJ_FILTER = conf
pha.adjust.filter-method = conf

# ADJ_EST = med
pha.adjust.est-method = med

# NS_LOOP = 1
pha.remove-insignificant = true

# === Station History Metadata ===
# SHF_META = 1
pha.use-history-files = 1

# === Logger Configuration ===
pha.logger.filename = pha-v52i-equivalent.log
pha.logger.level = INFO
pha.logger.print-to-stdout = true
pha.logger.append-datestamp = false
pha.logger.rollover-datestamp = false
```

---

## References

### v52i Source Files
- `phav52i/source_expand/inhomog.combo.mthly.incl` - Parameter definitions
- `phav52i/source_expand/inhomog.parm.system.MLY.incl` - System parameters
- `phav52i/source_expand/inhomog.parm.MTHLY.TEST.incl` - Test configuration
- `phav52i/source_expand/read_write.mthly.v6b.f` - Environment variable reading
- `phav52i/scripts/combo_runs/run_test.sh` - Example run script

### v4 Source Files
- `src/f95/PropertyParameters.f95` - Property name constants
- `src/f95/AlgorithmParameters.f95` - Algorithm constants (merge radii, etc.)
- `src/f95/PropertyReader.f95` - Property file parsing
- `src/f95/ConfigurationUtils.f95` - Property substitution

### Documentation
- Menne, M.J. and C.N. Williams, 2009: Homogenization of temperature series via pairwise comparisons. J. Climate, 22, 1700-1717.
- GHCN-Monthly v3 Technical Documentation
- GHCN-Monthly v4 Release Notes

---

## Version History

- **2026-01-26:** Initial comparison document created from v52i source code analysis
- **Source v52i:** stable-v52i branch (GHCNM v3 era implementation)
- **Source v4:** NOAA release 2025-03-20 (reconstructed)
