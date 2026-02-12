# Data Formats

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
