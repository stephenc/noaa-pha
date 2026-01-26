# GHCNM v4 Complete Processing Workflow

This document describes the complete workflow for processing climate station data from raw observations to homogenized, quality-controlled temperature records.

## Overview

The GHCNM processing pipeline transforms raw station observations into a globally consistent, homogeneous dataset suitable for climate analysis. The pipeline includes data merging, quality control, and homogenization steps.

## Full Production Pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                    RAW DATA SOURCES                                 │
├─────────────────────────────────────────────────────────────────────┤
│ • GHCN-Daily (daily obs aggregated to monthly)                     │
│ • GHCNMv2 legacy data                                               │
│ • CLIMAT messages (WMO monthly summaries)                          │
│ • National network data (e.g., USHCN)                              │
│ • Regional contributions                                            │
└─────────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────────┐
│               STAGE 1: DATA MERGING & CONVERSION                    │
├─────────────────────────────────────────────────────────────────────┤
│ Programs: merge_d2m_mv2, merge_update, stage3_to_ghcnm            │
│                                                                     │
│ Tasks:                                                              │
│ • Merge multiple data sources by station                           │
│ • Resolve conflicts (prioritize sources)                           │
│ • Convert to standard GHCNM format                                 │
│ • Create station inventory                                          │
│                                                                     │
│ Output: ghcnm.v4.0.0.{inv,dat} (Raw merged data)                  │
└─────────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────────┐
│           STAGE 2: METADATA UPDATES & CORRECTIONS                   │
├─────────────────────────────────────────────────────────────────────┤
│ Programs: ghcnm_edit, ghcnm_climat                                 │
│                                                                     │
│ Tasks:                                                              │
│ • Apply expert-identified corrections                               │
│ • Update station metadata (location, elevation)                    │
│ • Replace suspect data with CLIMAT observations                    │
│ • Fix known data errors                                            │
│                                                                     │
│ Input:  Edit files, CLIMAT data                                    │
│ Output: ghcnm.v4.0.1.{inv,dat}                                     │
└─────────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────────┐
│              STAGE 3: STATION FILTERING                             │
├─────────────────────────────────────────────────────────────────────┤
│ Program: ghcnm_clean                                                │
│                                                                     │
│ Tasks:                                                              │
│ • Remove stations with insufficient data                           │
│ • Apply expert keep/remove lists                                   │
│ • Mark stations suitable for PHA with asterisk                     │
│                                                                     │
│ Output: ghcnm.v4.0.2.{inv,dat}                                     │
└─────────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────────┐
│             STAGE 4: QUALITY CONTROL (QC)                           │
├─────────────────────────────────────────────────────────────────────┤
│ Program: ghcnm_qc                                                   │
│                                                                     │
│ Tasks:                                                              │
│ • Check for duplicate years                                        │
│ • Flag world record extremes                                       │
│ • Detect identical value streaks                                   │
│ • Spatial consistency checks (outliers vs neighbors)               │
│ • Internal consistency (TMAX >= TMIN)                              │
│                                                                     │
│ QC Flags Applied:                                                   │
│ • D = Duplicate year                                               │
│ • R = World record                                                 │
│ • K = Streak (too many identical values)                           │
│ • S/T = Spatial outlier                                            │
│ • I = Internal inconsistency                                       │
│                                                                     │
│ Output: ghcnm.qca.{inv,dat}                                        │
└─────────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────────┐
│    STAGE 5: PAIRWISE HOMOGENEITY ADJUSTMENT (PHA) - PHAMain        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Step 5A: Neighbor Selection                                       │
│  ┌────────────────────────────────────────────────────┐            │
│  │ For each target station:                           │            │
│  │ 1. Calculate distance to all other stations        │            │
│  │    (ushcn_dist.v6.combo)                           │            │
│  │ 2. Select N closest stations                       │            │
│  │ 3. Calculate correlation with each                 │            │
│  │    (ushcn_corr.v5a.combo)                          │            │
│  │ 4. Keep M best-correlated neighbors                │            │
│  └────────────────────────────────────────────────────┘            │
│                      ↓                                              │
│  Step 5B: Changepoint Detection                                    │
│  ┌────────────────────────────────────────────────────┐            │
│  │ For each target-neighbor pair:                     │            │
│  │ 1. Create difference series (target - neighbor)    │            │
│  │ 2. Apply SNHT test to find breaks                  │            │
│  │ 3. Use BIC to select best model                    │            │
│  │ 4. Determine break timing and magnitude            │            │
│  └────────────────────────────────────────────────────┘            │
│                      ↓                                              │
│  Step 5C: Changepoint Attribution                                  │
│  ┌────────────────────────────────────────────────────┐            │
│  │ Determine which station caused each break:         │            │
│  │ • Compare break across multiple neighbor pairs     │            │
│  │ • Vote: breaks appearing in multiple pairs         │            │
│  │   are attributed to the target station             │            │
│  │ • Consider station history metadata                │            │
│  └────────────────────────────────────────────────────┘            │
│                      ↓                                              │
│  Step 5D: Adjustment Calculation                                   │
│  ┌────────────────────────────────────────────────────┐            │
│  │ For each confirmed target break:                   │            │
│  │ 1. Calculate pairwise adjustments from neighbors   │            │
│  │ 2. Remove outliers (Tukey or confidence limits)    │            │
│  │ 3. Combine estimates (median/mean)                 │            │
│  │ 4. Apply adjustment to segment                     │            │
│  └────────────────────────────────────────────────────┘            │
│                      ↓                                              │
│  Step 5E: Write Output                                             │
│  ┌────────────────────────────────────────────────────┐            │
│  │ • Adjusted data files (*.WMs.{version}.{element})  │            │
│  │ • Adjustment metadata                               │            │
│  │ • Changepoint lists                                 │            │
│  │ • Diagnostic logs                                   │            │
│  └────────────────────────────────────────────────────┘            │
│                                                                     │
│ Output: Homogenized station files in output directory              │
└─────────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────────┐
│           STAGE 6: DERIVED PRODUCTS (Optional)                      │
├─────────────────────────────────────────────────────────────────────┤
│ Programs: gen_avg.3flg, gen_dtr.3flg, average_network              │
│                                                                     │
│ Tasks:                                                              │
│ • Calculate TAVG from (TMAX + TMIN) / 2                           │
│ • Calculate DTR (Diurnal Temperature Range)                        │
│ • Generate network/regional averages                               │
│ • Calculate anomalies                                               │
│                                                                     │
│ Output: Derived element files, averages                            │
└─────────────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────────────┐
│              STAGE 7: FORMAT & DISTRIBUTION                         │
├─────────────────────────────────────────────────────────────────────┤
│ Programs: convert.pha2cas, join_gv4                                │
│                                                                     │
│ Tasks:                                                              │
│ • Convert to public distribution formats                           │
│ • Combine station files into single dataset                        │
│ • Generate documentation                                            │
│ • Create archive files                                              │
│                                                                     │
│ Output: ghcnm.v4.x.x.{inv,dat} (Public release)                    │
└─────────────────────────────────────────────────────────────────────┘
```

## Simplified Workflow for Running PHA Only

If you already have quality-controlled data and just want to run the homogeneity adjustment:

```
┌──────────────────────┐
│  YOUR INPUT DATA     │
│ ─────────────────────│
│ • ghcnm.inv          │
│ • Station data files │
│ • (Optional) .his    │
└──────────────────────┘
          ↓
┌──────────────────────┐
│  1. Setup            │
│ ─────────────────────│
│ • Create directory   │
│   structure          │
│ • Configure          │
│   properties file    │
└──────────────────────┘
          ↓
┌──────────────────────┐
│  2. Run PHAMain      │
│ ─────────────────────│
│ ./bin/PHAMain        │
│                      │
│ Internally:          │
│ • Select neighbors   │
│ • Detect breaks      │
│ • Calculate adjust.  │
│ • Write output       │
└──────────────────────┘
          ↓
┌──────────────────────┐
│  OUTPUT              │
│ ─────────────────────│
│ • Adjusted data      │
│ • Neighbor files     │
│ • Changepoint lists  │
│ • Log files          │
└──────────────────────┘
```

## File Flow Through PHAMain

```
INPUT FILES:
├── ghcnm.inv                          Station metadata (required)
├── data/ghcnm_v4/raw/*.raw.tmax       Raw temperature data (required)
└── data/history/*.his                 Station history (optional)

PROCESSING:
├── [In memory] Distance calculations
├── [In memory] Correlation calculations
├── [In memory] Difference series
├── [In memory] Changepoint detection (SNHT)
└── [In memory] Adjustment estimates

INTERMEDIATE OUTPUT:
├── output/neighbors-distance.txt      Geographic neighbors
└── output/neighbors-correlation.txt   Correlation-selected neighbors

FINAL OUTPUT:
├── output/ghcnm_v4_adjusted/tmax/*.WMs.v1.tmax   Adjusted data
├── output/changepoints/                           Break lists
└── pha-tmax.log                                   Processing log
```

## Data Flow: Single Station Example

Let's trace station `USC00084731` through the PHA process:

```
STEP 1: READ INPUT
─────────────────────────────────────────────────────
ghcnm.inv:
USC00084731  40.7833 -111.9667 1288.1 SALT LAKE CITY INT AP

data/ghcnm_v4/raw/USC00084731.raw.tmax:
USC00084731 1950    134      267    ... (12 monthly values)
USC00084731 1951    178      456    ...
...

STEP 2: FIND NEIGHBORS
─────────────────────────────────────────────────────
Calculate distance to all other stations
→ Find 40 closest stations
→ Calculate correlation with each
→ Keep 20 best correlated

output/neighbors-correlation.txt:
USC00084731 USC00087266 USC00085628 USC00086928 ...
          1           2           3           4
       1.00        0.85        0.78        0.72

STEP 3: DETECT CHANGEPOINTS
─────────────────────────────────────────────────────
For USC00084731 vs each neighbor:
  Difference series = USC00084731 - USC00087266
  Run SNHT test → Find breaks at years: 1965, 1982

Repeat for all 20 neighbors
Aggregate results → Confirmed breaks: 1965, 1982

STEP 4: CALCULATE ADJUSTMENTS
─────────────────────────────────────────────────────
For break at 1965:
  Pairwise estimates from 20 neighbors: [0.8, 0.9, 0.7, ...]
  Remove outliers
  Take median → Adjustment = 0.8°C

Apply to all data before 1965

STEP 5: WRITE OUTPUT
─────────────────────────────────────────────────────
output/ghcnm_v4_adjusted/tmax/USC00084731.WMs.v1.tmax:
USC00084731 1950     54 A     187 A  ... (adjusted values)
USC00084731 1951     98 A     376 A  ...
...
USC00084731 1965    134      267    ... (original, post-break)
...

Flags: 'A' = Adjusted by PHA
```

## Configuration Decision Tree

```
What data do you have?
│
├─ Official GHCNM v4 download
│  └─→ Extract region with examples/prepare_sample_data.sh
│      └─→ Edit config: Set paths, element
│          └─→ Run PHAMain
│
├─ Raw station observations in custom format
│  └─→ Convert to GHCNM 3-flag format (see DATA_FORMAT_EXAMPLES.md)
│      └─→ Create inventory file
│          └─→ Edit config: Set paths, reduce neighbor limits
│              └─→ Run PHAMain
│
└─ Small test dataset (< 50 stations)
   └─→ Create test files manually
       └─→ Edit config:
           • neighbors.distance-neighbor-limit = 10
           • neighbors.final-neighbor-limit = 5
           • confirm = 1
           • adjust.min-neighbors = 3
       └─→ Run PHAMain
```

## Processing Order for Multiple Elements

If processing all three temperature elements:

```bash
# 1. Process TMAX
cp ghcnm-pha-tmax.properties ghcnm-pha.properties
./bin/PHAMain
mv pha.log pha-tmax.log

# 2. Process TMIN
cp ghcnm-pha-tmin.properties ghcnm-pha.properties
./bin/PHAMain
mv pha.log pha-tmin.log

# 3. Generate TAVG from adjusted TMAX and TMIN
./bin/gen_avg.3flg -v byear=1880 -v eyear=2024 \
    -v ddir=output/ghcnm_v4_adjusted \
    -v vproc=WMs.v1 \
    station_list.txt

# 4. Optionally run PHA on TAVG too
cp ghcnm-pha-tavg.properties ghcnm-pha.properties
./bin/PHAMain
```

## Validation Workflow

After running PHA, validate your results:

```
1. Check log file
   └─→ grep -i error pha.log
   └─→ grep "stations processed" pha.log

2. Verify output files created
   └─→ ls -lh output/ghcnm_v4_adjusted/tmax/ | wc -l
   └─→ Compare count to input stations

3. Examine sample adjusted series
   └─→ head -20 output/ghcnm_v4_adjusted/tmax/USC00084731.WMs.v1.tmax
   └─→ Check for 'A' flags (adjustments applied)
   └─→ Count adjustments: grep -o A output/.../USC00084731.WMs.v1.tmax | wc -l

4. Review changepoints detected
   └─→ Check changepoint output files
   └─→ Compare to known station history

5. Compare with reference
   └─→ If processing official data, compare some stations
       with GHCNM v4 public product
```

## Performance Considerations

### Processing Time Estimates

For reference (actual times vary by hardware):

- **100 stations**: ~1-5 minutes
- **1,000 stations**: ~30-60 minutes
- **10,000 stations**: ~10-20 hours
- **Full GHCNM (~27,000 stations)**: ~24-72 hours

Most time is spent in:
1. Neighbor correlation calculation (O(N²) station comparisons)
2. SNHT testing on all difference series
3. File I/O for thousands of station files

### Optimization Tips

**For faster processing:**
```properties
# Reduce neighbors checked
pha.neighbors.distance-neighbor-limit = 20  # Default: 40
pha.neighbors.final-neighbor-limit = 10     # Default: 20

# Use distance-only (skip correlation)
pha.neighbors.method = distance-only

# Process smaller regions separately
# Then combine results
```

**For better results (slower):**
```properties
# Check more neighbors
pha.neighbors.distance-neighbor-limit = 60
pha.neighbors.final-neighbor-limit = 30

# More stringent correlation
pha.neighbors.min-coefficient = 0.2

# More conservative changepoint detection
pha.confirm = 4
```

## Troubleshooting Workflow

```
Problem: PHAMain fails to start
├─→ Check: Properties file exists and is readable
├─→ Check: Properties file has no syntax errors
│   └─→ Look for lines without '='
│   └─→ Check for unclosed {braces}
└─→ Check: All required properties are set

Problem: "No neighbors found for station"
├─→ Check: Station inventory has enough stations
│   └─→ Need at least 5-10 neighbors within ~1000 km
├─→ Reduce: pha.neighbors.min-coefficient to 0.0
├─→ Increase: pha.neighbors.distance-neighbor-limit
└─→ Check: Input data files exist for neighbors

Problem: No adjustments applied
├─→ Check: pha.confirm requirement too high?
│   └─→ Reduce to 1 or 2 for testing
├─→ Check: SNHT threshold too strict?
│   └─→ Try 10 instead of 1
├─→ Check: Station records too short?
│   └─→ pha.adjust.min-length = 18 months minimum
└─→ Review: Log file for detection details

Problem: Too many adjustments
├─→ Increase: pha.confirm (make more conservative)
├─→ Decrease: pha.snht-threshold (1 is most strict)
├─→ Enable: pha.remove-insignificant = true
└─→ Check: Input data quality (run QC first)
```

## Next Steps

- **Quick Start**: [GETTING_STARTED.md](GETTING_STARTED.md)
- **Data Formats**: [examples/DATA_FORMAT_EXAMPLES.md](examples/DATA_FORMAT_EXAMPLES.md)
- **Complete Reference**: [README.md](README.md)
