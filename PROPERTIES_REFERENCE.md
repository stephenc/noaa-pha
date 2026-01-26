# GHCNM PHA Properties Reference

**Canonical reference for all configuration properties**

This document is generated from source code analysis of the PropertyParameters.f95 module and property usage throughout the codebase.

---

## Table of Contents

- [General Run Parameters](#general-run-parameters)
- [File Path Properties](#file-path-properties)
- [Neighbor Selection Parameters](#neighbor-selection-parameters)
- [Changepoint Detection Parameters](#changepoint-detection-parameters)
- [Adjustment Calculation Parameters](#adjustment-calculation-parameters)
- [Logger Configuration](#logger-configuration)
- [Testing/Development Properties](#testingdevelopment-properties)
- [Property Value Substitution](#property-value-substitution)

---

## General Run Parameters

### pha.version
**Type:** String
**Required:** Yes
**Valid Values:** Any alphanumeric string
**Default:** None (must be set)
**Purpose:** Software version identifier used in directory structure and output file names
**Example:** `v1`, `test`, `20260126`
**Used by:** AdjustSeries (output file naming)

### pha.begin-year
**Type:** Integer
**Required:** Yes
**Valid Values:** Any year >= 1000 (typically 1800-2000)
**Default:** None (must be set)
**Typical:** `1880` or `1851`
**Purpose:** The earliest year possibly present in the input data for any station. Used to allocate data arrays.
**Used by:** PHAMain
**Notes:** Must be <= earliest data year in your dataset. Earlier values waste memory but are safe.

### pha.element
**Type:** String (enumerated)
**Required:** Yes
**Valid Values:**
- `tmax` - Maximum temperature
- `tmin` - Minimum temperature
- `tavg` - Average/mean temperature
- `tdtr` - Diurnal temperature range

**Default:** None (must be set)
**Purpose:** The climate element being processed during a given run
**Used by:** ChooseNeighbors, ReadInputFiles, AdjustSeries
**Notes:** Process one element at a time. Run program separately for each element.

### pha.input-data-type
**Type:** String
**Required:** Yes
**Valid Values:** Any string (commonly `raw`, `tob`, `adj`)
**Default:** `raw`
**Typical:** `raw` (for initial processing)
**Purpose:** Identifier for the input data processing stage, used in directory and file naming
**Used by:** ReadInputFiles
**Notes:** Matches subdirectory name under data directory (e.g., `data/ghcnm_v4/raw/`)

### pha.neighbors.input-data-type
**Type:** String
**Required:** Yes
**Valid Values:** Any string (commonly `raw`, `tob`, `adj`)
**Default:** Same as `pha.input-data-type`
**Purpose:** Input data type specifically for neighbor correlation calculations
**Used by:** ChooseNeighbors
**Notes:** Separated from `pha.input-data-type` to match original PHA neighbors code design. Usually set to same value as `pha.input-data-type`.

---

## File Path Properties

All file paths support property value substitution (e.g., `{pha.element}` is replaced with the element value).

### pha.path.station-metadata
**Type:** String (file path)
**Required:** Yes
**Valid Values:** Path to a valid station inventory file
**Default:** None (must be set)
**Purpose:** Location of the station metadata/inventory file (.inv format)
**Used by:** ChooseNeighbors, ReadInputFiles
**Example:** `data/ghcnm.inv`
**Format:** See README.md § GHCNM Inventory Format

### pha.path.neighbors.distance
**Type:** String (file path)
**Required:** Yes (for output if running neighbor selection)
**Valid Values:** Any writable file path
**Default:** None (must be set)
**Purpose:** Location of the closest neighbors (by geographic distance) output file
**Used by:** ChooseNeighbors (writes)
**Example:** `output/neighbors/neighbors-distance.txt`
**Format:** See README.md § Neighbor Distance File Format
**Notes:** Written by ChooseNeighbors when `pha.neighbors.method` != `distance-only`

### pha.path.neighbors.correlation
**Type:** String (file path)
**Required:** Yes (for output if running neighbor selection)
**Valid Values:** Any writable file path
**Default:** None (must be set)
**Purpose:** Location of the best neighbors (by correlation) output file created by ChooseNeighbors
**Used by:** ChooseNeighbors (writes)
**Example:** `output/neighbors/neighbors-correlation.txt`
**Format:** See README.md § Neighbor Correlation File Format

### pha.path.neighbors.correlation-in
**Type:** String (file path)
**Required:** Yes (when running main PHA)
**Valid Values:** Path to an existing neighbor correlation file
**Default:** None (must be set)
**Purpose:** Location of pre-computed neighbor correlation file to use as input for PHA processing
**Used by:** ReadInputFiles
**Example:** `output/neighbors/neighbors-correlation.txt`
**Notes:** Often set to same value as `pha.path.neighbors.correlation` when running full pipeline

### pha.path.station-element-data-in
**Type:** String (directory path)
**Required:** Yes
**Valid Values:** Path to directory containing station data files
**Default:** None (must be set)
**Purpose:** Location of input data files for all target stations
**Used by:** ReadInputFiles
**Example:** `data/ghcnm_v4/{pha.input-data-type}/`
**Notes:** Directory should contain files named `{station_id}.{data_type}.{element}`

### pha.path.neighbors.station-element-data-in
**Type:** String (directory path)
**Required:** Yes
**Valid Values:** Path to directory containing station data files
**Default:** Same as `pha.path.station-element-data-in`
**Purpose:** Location of input data files for all stations (including neighbors), used by neighbor correlation code
**Used by:** ChooseNeighbors
**Example:** `data/ghcnm_v4/{pha.neighbors.input-data-type}/`

### pha.path.station-element-data-out
**Type:** String (directory path)
**Required:** Yes
**Valid Values:** Path to writable directory
**Default:** None (must be set)
**Purpose:** Location where adjusted output data files will be written
**Used by:** AdjustSeries
**Example:** `output/ghcnm_v4_adjusted/{pha.element}/`
**Notes:** Output files named `{station_id}.WMs.{version}.{element}`

### pha.path.station-history
**Type:** String (directory path)
**Required:** Only if `pha.use-history-files` != 0
**Valid Values:** Path to directory containing .his files
**Default:** None
**Purpose:** Location of station history metadata files (.his format)
**Used by:** ReadInputFiles
**Example:** `data/history/`
**Notes:** Files should be named `{station_id}.his`. Optional if not using station history.

---

## Neighbor Selection Parameters

These parameters control how neighboring stations are identified and selected.

### pha.neighbors.distance-neighbor-limit
**Type:** Integer
**Required:** Yes
**Valid Values:** 1 to 1000 (practical range: 10-100)
**Default:** None (must be set)
**Typical:** `40`
**Recommended:**
- Small datasets (<100 stations): `10-20`
- Regional (100-1000 stations): `20-40`
- Global (>1000 stations): `40-60`

**Purpose:** Number of closest stations to initially consider as potential neighbors for each target station (before correlation filtering)
**Used by:** ChooseNeighbors, ReadInputFiles, FindChangepoints
**Notes:**
- Higher values = more thorough but slower
- Must be <= total number of stations in network
- Algorithm subtracts 1 internally (first neighbor is the station itself)

### pha.neighbors.method
**Type:** String (enumerated)
**Required:** Yes
**Valid Values:**
- `first-diffs` - Use first differences for correlation calculation (recommended for temperature)
- `monthly-anomaly` - Use anomalies from monthly climatology
- `distance-only` - Skip correlation, use only geographic distance

**Default:** None (must be set)
**Typical:** `first-diffs`
**Purpose:** Method for calculating correlation between stations when selecting best neighbors
**Used by:** ChooseNeighbors
**Notes:**
- `first-diffs`: Best for temperature data, removes seasonal cycle and long-term trends
- `monthly-anomaly`: Alternative approach using climatological anomalies
- `distance-only`: Fastest but less accurate; uses only closest stations by distance

### pha.neighbors.min-coefficient
**Type:** Real (float)
**Required:** Yes
**Valid Values:** -1.0 to 1.0 (practical range: 0.0 to 1.0)
**Default:** None (must be set)
**Typical:** `0.1`
**Recommended:**
- High quality: `0.2` or higher
- Standard: `0.1`
- Sparse networks: `0.0` (accept all)

**Purpose:** Minimum correlation coefficient threshold required to qualify as a neighbor
**Used by:** ChooseNeighbors
**Notes:**
- Set to `0.0` for sparse datasets to accept all neighbors
- Higher values ensure better quality neighbors but may exclude needed stations
- Typical correlations range from 0.3-0.9 for good neighbors

### pha.neighbors.min-station-coverage
**Type:** Integer
**Required:** Yes
**Valid Values:** 1 to `pha.neighbors.final-neighbor-limit`
**Default:** None (must be set)
**Typical:** `7` (for large networks), `3` (for small)
**Purpose:** Minimum number of neighbors with overlapping data required for a given time period to meet spatial coverage threshold
**Used by:** ChooseNeighbors
**Notes:** Used in spatial consistency checks. Lower values needed for sparse networks.

### pha.neighbors.final-neighbor-limit
**Type:** Integer
**Required:** Yes
**Valid Values:** 1 to `pha.neighbors.distance-neighbor-limit`
**Default:** None (must be set)
**Typical:** `20`
**Recommended:**
- Small datasets: `5-10`
- Regional: `10-20`
- Global: `20-30`

**Purpose:** Maximum number of best-correlated neighbors to retain in final neighbor list for each target station
**Used by:** ChooseNeighbors, ReadInputFiles
**Notes:**
- Must be <= `pha.neighbors.distance-neighbor-limit`
- More neighbors = more robust but slower processing
- Algorithm subtracts 1 internally

---

## Changepoint Detection Parameters

These parameters control how breaks/changepoints are detected and confirmed.

### pha.use-history-files
**Type:** Integer (enumerated)
**Required:** Yes
**Valid Values:**
- `-1` - Use station history ONLY, do not detect undocumented changepoints
- `0` - IGNORE history files, only use statistical detection
- `1` - Use history AND detect undocumented changepoints (recommended)

**Default:** None (must be set)
**Typical:** `1` or `0`
**Purpose:** Controls use of documented station metadata (moves, instrument changes) in determining timing of shifts
**Used by:** ReadInputFiles, FindChangepoints, MergeChangepoints, AttributeChangepoints
**Notes:**
- `-1`: Only apply adjustments at documented dates (requires good metadata)
- `0`: Full blind detection (use when metadata is poor/unavailable)
- `1`: Best of both - finds documented AND undocumented breaks

### pha.snht-threshold
**Type:** Integer (enumerated)
**Required:** Yes
**Valid Values:**
- `1` - 97.5% significance level (most strict, fewest breaks detected)
- `5` - 95% significance level (balanced)
- `10` - 90% significance level (most sensitive, most breaks detected)

**Default:** None (must be set)
**Typical:** `5`
**Purpose:** SNHT (Standard Normal Homogeneity Test) statistical significance threshold percentage
**Used by:** FindChangepoints
**Notes:**
- Lower number = stricter test = fewer changepoints detected
- `1`: Most conservative, may miss real breaks
- `10`: Most liberal, may create false positives
- `5`: Recommended balance

### pha.bic-penalty
**Type:** String (enumerated)
**Required:** Yes
**Valid Values:**
- `bic` - Bayesian Information Criterion (recommended)
- `aic` - Akaike Information Criterion
- `none` - No model selection penalty

**Default:** None (must be set)
**Typical:** `bic`
**Purpose:** Penalty function type used to determine the optimal number and form of changepoints in a series
**Used by:** ModelFitUtils
**Notes:**
- BIC penalizes model complexity more than AIC
- `bic`: More conservative, prevents overfitting
- `aic`: More liberal, may fit more changepoints
- `none`: No penalty, likely to overfit

### pha.amploc-percent
**Type:** Integer (enumerated)
**Required:** Yes
**Valid Values:**
- `90` - 90% confidence limit
- `92` - 92.5% confidence limit (recommended)
- `95` - 95% confidence limit

**Default:** None (must be set)
**Typical:** `92`
**Purpose:** Confidence limit used to quantify timing uncertainty of a changepoint (used when merging closely-timed changepoints)
**Used by:** MergeChangepoints, AlgorithmParameters
**Notes:**
- Higher values = wider merge window = fewer final changepoints
- Affects MERGE_RADII lookup in AlgorithmParameters.f95
- `92`: Recommended default from algorithm optimization

### pha.confirm
**Type:** Integer
**Required:** Yes
**Valid Values:** 1 to `pha.neighbors.final-neighbor-limit`
**Default:** None (must be set)
**Typical:** `3`
**Recommended:**
- High confidence: `4-5`
- Balanced: `3`
- Sparse networks: `1-2`

**Purpose:** Minimum number of target-neighbor difference series that must show coincident changepoints to confirm attribution to the target station
**Used by:** MergeChangepoints, AttributeChangepoints
**Notes:**
- Higher values = more conservative = fewer confirmed breaks
- Must be < number of neighbors available
- With 20 neighbors, `3` means at least 3 pairs must agree

---

## Adjustment Calculation Parameters

These parameters control how adjustment magnitudes are estimated and applied.

### pha.adjust.min-length
**Type:** Integer
**Required:** Yes
**Valid Values:** 1 to 1200 (practical range: 12-60)
**Default:** None (must be set)
**Typical:** `18` months
**Purpose:** Minimum length of a data segment (in months) that can have an adjustment applied
**Used by:** FindChangepoints, ModelFitUtils, ChangepointSize
**Notes:**
- Segments shorter than this are merged with adjacent segments
- `18` months = 1.5 years minimum
- Shorter values allow more granular adjustments but may be unstable

### pha.adjust.min-neighbors
**Type:** Integer
**Required:** Yes
**Valid Values:** 1 to `pha.neighbors.final-neighbor-limit`
**Default:** None (must be set)
**Typical:** `5`
**Recommended:**
- High confidence: `7-10`
- Balanced: `5`
- Sparse networks: `3`

**Purpose:** Minimum number of pairwise changepoint estimates required to determine the size of an adjustment
**Used by:** ChangepointSize
**Notes:**
- More neighbors = more robust estimate
- Fewer than this → no adjustment applied (segment kept unadjusted)
- Must be <= actual number of neighbors with data at the break

### pha.adjust.remove-outliers
**Type:** Logical (boolean)
**Required:** Yes
**Valid Values:**
- `true`, `.true.`, `t`, `yes`, `1` - Remove outliers
- `false`, `.false.`, `f`, `no`, `0` - Keep all estimates

**Default:** None (must be set)
**Typical:** `true`
**Purpose:** Whether to test and remove outlier pairwise estimates using Tukey outlier test before calculating adjustment magnitude
**Used by:** ChangepointSize
**Notes:**
- `true`: More robust, removes extreme estimates
- `false`: Uses all neighbor estimates (may be biased by outliers)
- Outlier removal happens before estimating final adjustment

### pha.adjust.window
**Type:** Integer
**Required:** Yes
**Valid Values:** 0 to 240 (practical range: 0, 12-60)
**Default:** None (must be set)
**Typical:** `0` (no limit)
**Purpose:** Number of months before and after a break to use when calculating adjustment size (0 = use all available data in each segment)
**Used by:** ChangepointSize
**Notes:**
- `0`: Use entire segment (recommended)
- Positive value: Limit to N months around break (may be more stable but uses less data)
- Window applies to both sides of break

### pha.adjust.filter-method
**Type:** String (enumerated)
**Required:** Yes
**Valid Values:**
- `conf` - Tukey confidence limit variant (recommended)
- `bicf` - Bayesian Information Criterion filtering
- `both` - Apply both Tukey and BIC filters
- `none` - No filtering of pairwise estimates

**Default:** None (must be set)
**Typical:** `conf`
**Purpose:** Outlier filtering method for pairwise changepoint adjustment estimates
**Used by:** ChangepointSize
**Notes:**
- `conf`: Uses interquartile range method (Tukey variant)
- `bicf`: Model-based filtering
- `both`: Most conservative, removes most outliers
- `none`: No filtering (not recommended)

### pha.adjust.est-method
**Type:** String (enumerated)
**Required:** Yes
**Valid Values:**
- `med` - Median of pairwise estimates (recommended, most robust)
- `avg` - Mean/average of pairwise estimates
- `qav` - Inter-quartile range average (trimmed mean)

**Default:** None (must be set)
**Typical:** `med`
**Purpose:** Method used to combine multiple pairwise estimates into a single adjustment value
**Used by:** ChangepointSize
**Notes:**
- `med`: Most robust to outliers
- `avg`: Sensitive to outliers but uses all data
- `qav`: Compromise between median and mean

### pha.remove-insignificant
**Type:** Logical (boolean)
**Required:** Yes
**Valid Values:**
- `true` - Merge segments when adjustment is statistically insignificant
- `false` - Keep all detected changepoints

**Default:** None (must be set)
**Typical:** `true`
**Purpose:** Whether to merge adjacent data segments when the changepoint adjustment size is statistically insignificant
**Used by:** ChangepointSize
**Notes:**
- `true`: Reduces number of changepoints, removes noise
- `false`: Keeps all detected breaks (may include insignificant ones)
- Significance test based on uncertainty estimates

---

## Logger Configuration

Controls the logging output and verbosity.

### pha.logger.filename
**Type:** String (file path)
**Required:** Yes
**Valid Values:** Any writable file path
**Default:** None (must be set)
**Typical:** `pha.log` or `pha-{pha.element}.log`
**Purpose:** Base name of the logger output file
**Used by:** PHAMain (log_init)
**Example:** `build/pha-unit-test.log`
**Notes:** Date stamp may be appended if `pha.logger.append-datestamp` is true

### pha.logger.level
**Type:** String (enumerated)
**Required:** Yes
**Valid Values:**
- `DEBUG` - Most verbose, all messages
- `INFO` - Informational messages and above
- `WARN` - Warnings and errors only
- `ERROR` - Errors and fatal only
- `FATAL` - Fatal errors only

**Default:** None (must be set)
**Typical:** `INFO` (for production), `DEBUG` (for troubleshooting)
**Purpose:** Minimum logging level to write to file
**Used by:** PHAMain (log_init)
**Notes:** Each level includes all more severe levels

### pha.logger.print-to-stdout
**Type:** Logical (boolean)
**Required:** Yes
**Valid Values:** `true` or `false`
**Default:** None (must be set)
**Typical:** `true` (to see progress), `false` (batch jobs)
**Purpose:** Whether to also print log messages to standard output (console)
**Used by:** PHAMain (log_init)

### pha.logger.append-datestamp
**Type:** Logical (boolean)
**Required:** Yes
**Valid Values:** `true` or `false`
**Default:** None (must be set)
**Typical:** `false`
**Purpose:** Whether to append a datestamp to the log filename
**Used by:** PHAMain (log_init)
**Notes:** If true, creates files like `pha.log.20260126`

### pha.logger.rollover-datestamp
**Type:** Logical (boolean)
**Required:** Yes
**Valid Values:** `true` or `false`
**Default:** None (must be set)
**Typical:** `false`
**Purpose:** Whether to create a new log file with updated datestamp after midnight
**Used by:** PHAMain (log_init)
**Notes:** Useful for long-running processes spanning multiple days

---

## Testing/Development Properties

These properties are used for testing and development. Not needed for normal operation.

### pha.do-run-neighbors
**Type:** Logical (boolean)
**Required:** No (testing only)
**Valid Values:** `true` or `false`
**Default:** `true` (if not set)
**Purpose:** Controls whether to run the neighbor selection code
**Used by:** PHAMain
**Notes:** Set to `false` to skip neighbor calculation if using pre-computed neighbors

### pha.do-run-main
**Type:** Logical (boolean)
**Required:** No (testing only)
**Valid Values:** `true` or `false`
**Default:** `true` (if not set)
**Purpose:** Controls whether to run the main PHA processing
**Used by:** PHAMain
**Notes:** Set to `false` to only run neighbor selection

### pha.test.*
**Type:** Various
**Required:** No (unit tests only)
**Purpose:** Properties used specifically for unit testing
**Used by:** ConfigurationUtilsTest, PHATestUnits
**Examples:**
- `pha.test.foo-foo-bar`
- `pha.test.use-arg`
- `pha.test.long-flag`

**Notes:** Only needed when running `PHATestUnits`. See `build/ghcnm-pha.unit-test.properties`.

### pha.path.neighbors-distance-expected
**Type:** String (file path)
**Required:** No (testing only)
**Purpose:** Path to expected neighbor-distance output for comparison testing
**Used by:** PHATestOutput

### pha.path.neighbors-correlation-expected
**Type:** String (file path)
**Required:** No (testing only)
**Purpose:** Path to expected neighbor-correlation output for comparison testing
**Used by:** PHATestOutput

### pha.path.network-output-expected-dir
**Type:** String (directory path)
**Required:** No (testing only)
**Purpose:** Directory containing expected output files for comparison testing
**Used by:** PHATestOutput

---

## Property Value Substitution

Properties support runtime variable substitution using tokens in curly braces `{}`.

### Property References
Reference other properties using `{property.name}`:

```properties
pha.element = tmax
pha.logger.filename = logs/pha-{pha.element}.log
# Result: logs/pha-tmax.log
```

Common substitutions:
```properties
{pha.element}                 # Replaced with current element
{pha.version}                 # Replaced with version string
{pha.input-data-type}         # Replaced with data type
{pha.neighbors.input-data-type}
```

### Command-Line Arguments
Reference command-line arguments using `{arg:flag}`:

```properties
pha.test.use-arg = use-{arg:d}
# When run with: PHATestUnits -d 20160316
# Result: use-20160316
```

### Nested Substitution
Properties are resolved iteratively up to 5 levels deep:

```properties
pha.input-data-type = raw
pha.path.base = data/ghcnm_v4/{pha.input-data-type}/
pha.path.station-element-data-in = {pha.path.base}{pha.element}/
# Final result: data/ghcnm_v4/raw/tmax/
```

### Substitution Order
1. Properties file is read
2. All `{arg:flag}` tokens replaced with command-line values
3. All `{property.name}` tokens replaced iteratively
4. Properties with substitutions marked "Overwriting property..." in log

---

## Complete Minimal Configuration

Absolute minimum properties needed to run PHAMain:

```properties
# Core
pha.begin-year = 1880
pha.element = tmax
pha.input-data-type = raw
pha.neighbors.input-data-type = raw
pha.version = v1

# Files
pha.path.station-metadata = data/ghcnm.inv
pha.path.neighbors.distance = output/neighbors-distance.txt
pha.path.neighbors.correlation = output/neighbors-correlation.txt
pha.path.neighbors.correlation-in = output/neighbors-correlation.txt
pha.path.station-element-data-in = data/ghcnm_v4/raw/
pha.path.neighbors.station-element-data-in = data/ghcnm_v4/raw/
pha.path.station-element-data-out = output/adjusted/tmax/
pha.path.station-history = data/history/

# Neighbors
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.method = first-diffs
pha.neighbors.min-coefficient = 0.1
pha.neighbors.min-station-coverage = 7
pha.neighbors.final-neighbor-limit = 20

# Detection
pha.use-history-files = 0
pha.snht-threshold = 5
pha.bic-penalty = bic
pha.amploc-percent = 92
pha.confirm = 3

# Adjustment
pha.adjust.min-length = 18
pha.adjust.min-neighbors = 5
pha.adjust.remove-outliers = true
pha.adjust.window = 0
pha.adjust.filter-method = conf
pha.adjust.est-method = med
pha.remove-insignificant = true

# Logging
pha.logger.filename = pha.log
pha.logger.level = INFO
pha.logger.print-to-stdout = true
pha.logger.append-datestamp = false
pha.logger.rollover-datestamp = false
```

---

## Properties by Source Code Module

| Property | Defined in | Read by |
|----------|-----------|---------|
| All PROP_* constants | PropertyParameters.f95 | All |
| General run params | PropertyParameters.f95 | PHAMain, ReadInputFiles, AdjustSeries |
| File paths | PropertyParameters.f95 | ChooseNeighbors, ReadInputFiles, AdjustSeries |
| Neighbor params | PropertyParameters.f95 | ChooseNeighbors, ReadInputFiles, FindChangepoints |
| Detection params | PropertyParameters.f95 | FindChangepoints, MergeChangepoints, ModelFitUtils |
| Adjustment params | PropertyParameters.f95 | ChangepointSize, FindChangepoints |
| Logger params | PropertyParameters.f95 | PHAMain |
| Substitution | ConfigurationUtils.f95 | All via detokenize_all_properties() |

---

## Version History

- **2026-01-26:** Initial canonical reference created from source code audit
- **Source:** GHCNM v4 reconstructed (2025-03-20 NOAA release)
- **Compiler:** gfortran with Fortran 95/2003 features

---

## See Also

- [GETTING_STARTED.md](GETTING_STARTED.md) - Practical configuration guide
- [examples/DATA_FORMAT_EXAMPLES.md](examples/DATA_FORMAT_EXAMPLES.md) - Properties file format examples
- [QUICK_REFERENCE.md](QUICK_REFERENCE.md) - Quick property lookup
- PropertyParameters.f95 - Source code defining all property constants
- PropertyReader.f95 - Property file parsing implementation
- ConfigurationUtils.f95 - Property substitution implementation
