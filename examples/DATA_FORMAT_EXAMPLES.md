# GHCNM Data Format Examples

This guide shows real examples of the data formats used by the PHA software.

## Station Inventory File Format

**File name:** `ghcnm.inv` or `*.inv`

**Purpose:** Contains metadata for each weather station

**Format:**
```
Columns  1-11: Station ID (11 characters)
Columns 13-20: Latitude (decimal degrees, F8.4)
Columns 22-30: Longitude (decimal degrees, F9.4)
Columns 32-37: Elevation (meters, F6.1)
Columns 39-68: Station Name (30 characters)
Columns 70+:   Optional metadata (composite IDs, flags)
```

**Example:**
```
USH00018323  31.8075  -85.9722  165.2 AL TROY                            USC00018323 ----------- ----------- ----------- +6
10160475000  35.4800    8.1300   67.0 DUMMY-STATION
ACW00011604  17.1167  -61.7833   10.1 ST JOHNS COOLIDGE FLD
USC00084731  40.7833 -111.9667 1288.1 SALT LAKE CITY INT AP
```

**Parsing in shell:**
```bash
# Extract just station IDs
cut -c1-11 ghcnm.inv

# Find stations in a region (lat 30-40, lon -120 to -100)
awk '$2 >= 30 && $2 <= 40 && $3 >= -120 && $3 <= -100' ghcnm.inv

# Count total stations
wc -l ghcnm.inv
```

## Monthly Temperature Data Format (3-Flag)

**File name:** `{station_id}.{process}.{element}`
- Example: `USC00084731.raw.tmax`

**Purpose:** Monthly temperature values with quality flags

**Format:**
```
Columns  1-11: Station ID (11 characters)
Column   12:   Space
Columns 13-16: Year (4 digits)
Columns 17-25: January value (I6) + 3 flag characters
Columns 26-34: February value (I6) + 3 flag characters
... (repeat for each month)
Columns 116-124: December value (I6) + 3 flag characters
```

**Value:**
- Integer in hundredths of degrees Celsius
- -9999 = missing data
- Example: 1234 = 12.34°C

**Flags (3 characters per month):**
1. **DM flag** (Data Measurement): How value was measured
   - (blank) = Standard measurement
   - `E` = Estimated/Filled by PHA
   - Other codes for special observations

2. **QC flag** (Quality Control): Quality assessment
   - (blank) = Passed all checks
   - `X` = Failed quality control, removed
   - `Q` = Questionable, flagged by QC
   - Other codes for specific QC failures

3. **DS flag** (Data Source): Origin of the data
   - (blank) = Standard source
   - Various codes indicating data source

**Example file** (`USC00084731.raw.tmax`):
```
USC00084731 1950    134      267      589     1156     1789     2567     3078     2978     2456     1567      722      289
USC00084731 1951    178      456      789     1456     2089     2789     3189     3067     2678     1789      856      345
USC00084731 1952   -9999    -9999    -9999    -9999    -9999    -9999    -9999    -9999    -9999    -9999    -9999    -9999
USC00084731 1953    145E     312E     678      1245     1945     2634     3123     3012     2567     1634      789      312
USC00084731 1954    123 X    345      701     1356     2012     2701     3145     3023     2612     1701      812      334
```

**Reading the example:**
- Line 1, January: Value=134 (1.34°C), no flags
- Line 1, February: Value=267 (2.67°C), no flags
- Line 3: Entire year missing (-9999)
- Line 4, January: Value=145 (1.45°C), DM flag='E' (estimated)
- Line 5, January: Value=123 (1.23°C), QC flag='X' (failed QC, removed)

**Creating a data file programmatically:**

Python example:
```python
def write_ghcnm_record(f, station_id, year, values, flags):
    """
    Write one year of data in GHCNM 3-flag format

    Args:
        f: file handle
        station_id: 11-character station ID
        year: 4-digit year
        values: list of 12 monthly values (integers, -9999 for missing)
        flags: list of 12 tuples of (dm, qc, ds) flags (single chars)
    """
    line = f"{station_id} {year:4d}"
    for val, (dm, qc, ds) in zip(values, flags):
        line += f"{val:6d}{dm}{qc}{ds}"
    f.write(line + "\n")

# Example usage:
with open("USC00084731.raw.tmax", "w") as f:
    values = [134, 267, 589, 1156, 1789, 2567, 3078, 2978, 2456, 1567, 722, 289]
    flags = [(' ', ' ', ' ')] * 12  # All blank flags
    write_ghcnm_record(f, "USC00084731", 1950, values, flags)
```

AWK example:
```bash
# Extract just the temperature values (no flags) from a file
awk '{
    printf "%s %s", $1, $2
    for (i = 3; i <= NF; i++) {
        val = substr($i, 1, 6)  # First 6 characters are the value
        printf " %6d", val
    }
    printf "\n"
}' USC00084731.raw.tmax
```

## Neighbor Distance File Format

**File name:** `neighbors-distance.txt`

**Purpose:** Pre-computed nearest neighbors for each station by geographic distance

**Format:** 3 lines per target station:
```
Line 1: Target ID, Neighbor1 ID, Neighbor2 ID, ... (space-separated, 11 chars each)
Line 2: Target index, Neighbor1 index, Neighbor2 index, ... (space-separated, I11)
Line 3: 0.0, Distance1 (km), Distance2 (km), ... (space-separated, F11.1)
```

**Example:**
```
USC00084731 USC00087266 USC00085628 USW00024127 USC00086440
          1           2           3           4           5
        0.0        15.3        28.7        45.2        62.8
USW00024127 USC00084731 USC00087266 USC00086928 USC00085628
          4           1           2           6           3
        0.0        45.2        52.1        67.3        71.5
```

**Reading:**
- USC00084731's nearest neighbor is USC00087266 at 15.3 km
- USC00084731's 2nd nearest is USC00085628 at 28.7 km
- USW00024127's nearest neighbor is USC00084731 at 45.2 km

## Neighbor Correlation File Format

**File name:** `neighbors-correlation.txt`

**Purpose:** Best neighbors selected based on correlation (subset of distance neighbors)

**Format:** 3 lines per target station (same structure as distance file):
```
Line 1: Target ID, Neighbor IDs... (sorted by correlation, highest first)
Line 2: Indices
Line 3: 1.00, Correlation1, Correlation2, ... (1.00 = perfect correlation with self)
```

**Example:**
```
USC00084731 USC00087266 USW00024127 USC00085628 USC00086440
          1           2           4           3           5
       1.00        0.85        0.78        0.72        0.65
```

**Reading:**
- USC00084731 has best correlation (0.85) with USC00087266
- Correlations are sorted highest to lowest
- First value is always 1.00 (station correlated with itself)

## Properties File Format

**File name:** `ghcnm-pha.properties`

**Purpose:** Configuration for PHAMain and other programs

**Format:** Java-style properties (key=value)

**Example:**
```properties
# Comments start with #

# === Logger Configuration ===
pha.logger.filename = pha-tmax.log
pha.logger.level = INFO
pha.logger.print-to-stdout = true

# === Core Parameters ===
pha.begin-year = 1880
pha.element = tmax
pha.input-data-type = raw
pha.version = v1

# === File Paths ===
pha.path.station-metadata = data/ghcnm.inv
pha.path.station-element-data-in = data/ghcnm_v4/{pha.input-data-type}/
pha.path.station-element-data-out = output/adjusted/{pha.element}/

# === Neighbor Parameters ===
pha.neighbors.distance-neighbor-limit = 40
pha.neighbors.method = first-diffs
pha.neighbors.min-coefficient = 0.1
pha.neighbors.final-neighbor-limit = 20

# === Detection Parameters ===
pha.snht-threshold = 5
pha.confirm = 3
```

**Property Substitution:**
The software supports variable substitution:
- `{pha.element}` is replaced with the value of `pha.element`
- `{pha.input-data-type}` is replaced with the value of `pha.input-data-type`

Example:
```properties
pha.element = tmax
pha.input-data-type = raw
# This path becomes: data/ghcnm_v4/raw/
pha.path.station-element-data-in = data/ghcnm_v4/{pha.input-data-type}/
```

## Data Preparation Scripts

### Converting from GHCNM v4 official format

The official GHCNM v4 `.dat` files have a different format. Here's how to convert:

**Official format:**
```
Columns 1-11: Station ID
Columns 12-15: Year
Columns 16-19: Element (TMAX, TMIN, TAVG, etc.)
Columns 20-27: January (value I5 + flags AAA)
Columns 28-35: February (value I5 + flags AAA)
... (repeat for each month)
```

**Conversion script:**
```bash
#!/bin/bash
# Convert official GHCNM .dat to individual station files

ELEMENT="TMAX"
INPUT_FILE="ghcnm.v4.0.1.dat"
OUTPUT_DIR="data/ghcnm_v4/raw"

mkdir -p "$OUTPUT_DIR"

# Extract all unique station IDs
cut -c1-11 "$INPUT_FILE" | sort -u > station_ids.txt

# For each station, extract its records
while read station_id; do
    outfile="${OUTPUT_DIR}/${station_id}.raw.${ELEMENT,,}"  # lowercase element

    # Extract records for this station and element
    grep "^${station_id}" "$INPUT_FILE" | \
    awk -v elem="$ELEMENT" '$3 == elem' | \
    awk '{
        # Reformat: Keep station ID and year, then extract 12 months
        printf "%s %s", $1, $2
        for (i = 4; i <= 15; i++) {
            printf " %s", $i
        }
        printf "\n"
    }' > "$outfile"

    lines=$(wc -l < "$outfile")
    if [ "$lines" -gt 0 ]; then
        echo "Created $outfile with $lines years"
    else
        rm "$outfile"  # Remove empty files
    fi
done < station_ids.txt

rm station_ids.txt
```

## Testing Your Data Files

Use these commands to verify your data files are correctly formatted:

```bash
# Check station inventory format
# Should show 11-char IDs, followed by numeric lat/lon
head -5 data/ghcnm.inv

# Check a data file format
# Each line should be: 11-char ID, space, 4-digit year, then 12 monthly values+flags
head -5 data/ghcnm_v4/raw/USC00084731.raw.tmax

# Count years of data for a station
wc -l data/ghcnm_v4/raw/USC00084731.raw.tmax

# Check for missing data (-9999)
grep -c -- "-9999" data/ghcnm_v4/raw/USC00084731.raw.tmax

# Verify all files have correct naming
ls data/ghcnm_v4/raw/*.raw.tmax | head -10

# Count total station data files
ls data/ghcnm_v4/raw/*.raw.tmax | wc -l
```

## Common Issues

### "Invalid station ID"
- Station IDs must be exactly 11 characters
- Pad with spaces if needed: `"USC1234    "` (not just `"USC1234"`)

### "Format error reading data"
- Each month needs exactly 9 characters: 6 for value + 3 for flags
- Missing data: `-9999` followed by 3 spaces or flags
- Check for tab characters (should be spaces)

### "Property not found"
- Check properties file uses `=` not `:`
- No spaces around `=` (good: `key=value`, bad: `key = value`)
- Properties are case-sensitive

### "File not found"
- Check path separators (use `/` not `\`)
- Paths are relative to where you run `PHAMain`
- Use absolute paths if uncertain
