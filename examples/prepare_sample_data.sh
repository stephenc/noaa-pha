#!/bin/bash
# Example script to download and prepare a small sample dataset for testing PHA
# This script downloads official GHCNM v4 data and extracts a regional subset

set -e  # Exit on error

echo "=== GHCNM PHA Sample Data Preparation ==="
echo ""

# Configuration
REGION="US_Southwest"
LAT_MIN=30.0
LAT_MAX=40.0
LON_MIN=-120.0
LON_MAX=-110.0
ELEMENT="tmax"
DATA_URL="ftp://ftp.ncei.noaa.gov/pub/data/ghcn/v4"

# Create directory structure
echo "Creating directory structure..."
mkdir -p data/ghcnm_v4/raw
mkdir -p output/neighbors
mkdir -p output/ghcnm_v4_adjusted/${ELEMENT}
mkdir -p temp

# Download GHCNM v4 data
echo ""
echo "Downloading GHCNM v4 ${ELEMENT} data..."
if [ ! -f "temp/ghcnm.${ELEMENT}.latest.qcu.tar.gz" ]; then
    wget -P temp "${DATA_URL}/ghcnm.${ELEMENT}.latest.qcu.tar.gz" || {
        echo "ERROR: Failed to download data. Check your internet connection."
        echo "You may need to download manually from:"
        echo "  ${DATA_URL}/ghcnm.${ELEMENT}.latest.qcu.tar.gz"
        exit 1
    }
else
    echo "Data already downloaded, skipping..."
fi

# Extract the archive
echo "Extracting archive..."
cd temp
tar -xzf ghcnm.${ELEMENT}.latest.qcu.tar.gz
cd ..

# Find the extracted files (version number may vary)
INV_FILE=$(ls temp/ghcnm.${ELEMENT}.*.inv 2>/dev/null | head -1)
DAT_FILE=$(ls temp/ghcnm.${ELEMENT}.*.dat 2>/dev/null | head -1)

if [ -z "$INV_FILE" ] || [ -z "$DAT_FILE" ]; then
    echo "ERROR: Could not find extracted .inv or .dat files"
    exit 1
fi

echo "Found inventory: $INV_FILE"
echo "Found data file: $DAT_FILE"

# Extract regional subset from inventory
echo ""
echo "Extracting stations in region:"
echo "  Latitude: ${LAT_MIN} to ${LAT_MAX}"
echo "  Longitude: ${LON_MIN} to ${LON_MAX}"

awk -v lat_min=$LAT_MIN -v lat_max=$LAT_MAX \
    -v lon_min=$LON_MIN -v lon_max=$LON_MAX \
    '$2 >= lat_min && $2 <= lat_max && $3 >= lon_min && $3 <= lon_max' \
    "$INV_FILE" > data/ghcnm_${REGION}.inv

STATION_COUNT=$(wc -l < data/ghcnm_${REGION}.inv)
echo "Found ${STATION_COUNT} stations in region"

if [ "$STATION_COUNT" -lt 5 ]; then
    echo "WARNING: Very few stations found. You may need to adjust the region boundaries."
fi

# Extract station IDs
cut -c1-11 data/ghcnm_${REGION}.inv > temp/station_ids.txt

echo ""
echo "Extracting data for ${STATION_COUNT} stations..."
echo "This may take a few minutes..."

# Extract data for each station
# GHCNM .dat format: Each line has station ID (11 chars), year, element, 12 monthly values+flags
while IFS= read -r station_id; do
    outfile="data/ghcnm_v4/raw/${station_id}.raw.${ELEMENT}"

    # Extract all lines for this station
    # Format: stationID year element value1 flag1 value2 flag2 ... value12 flag12
    # We need to reformat to the 3-flag monthly format used by PHA
    grep "^${station_id}" "$DAT_FILE" | \
    awk -v sid="$station_id" '{
        printf "%s %4d", sid, $2
        # Process 12 months - each month has value (I5) + 3 flags (A1 each)
        for (i = 1; i <= 12; i++) {
            val_pos = 4 + (i-1)*8  # Starting position for each month
            value = $val_pos
            dmflag = $(val_pos+1)
            qcflag = $(val_pos+2)
            dsflag = $(val_pos+3)
            printf " %6d%c%c%c", value, dmflag, qcflag, dsflag
        }
        printf "\n"
    }' > "$outfile"

    lines=$(wc -l < "$outfile")
    if [ "$lines" -gt 0 ]; then
        echo "  ${station_id}: ${lines} years"
    fi
done < temp/station_ids.txt

# Create a sample properties file
echo ""
echo "Creating sample configuration file..."
cat > ghcnm-pha-${REGION}.properties << EOF
# PHA Configuration for ${REGION} sample
# Generated: $(date)

# === Logger Configuration ===
pha.logger.filename = pha-${REGION}-${ELEMENT}.log
pha.logger.level = INFO
pha.logger.print-to-stdout = true
pha.logger.append-datestamp = false

# === Core PHA Run Parameters ===
pha.begin-year = 1880
pha.element = ${ELEMENT}
pha.input-data-type = raw
pha.neighbors.input-data-type = raw
pha.version = ${REGION}

# === File Path Configuration ===
pha.path.station-metadata = data/ghcnm_${REGION}.inv
pha.path.neighbors.distance = output/neighbors/neighbors-distance-${REGION}.txt
pha.path.neighbors.correlation = output/neighbors/neighbors-correlation-${REGION}.txt
pha.path.station-element-data-in = data/ghcnm_v4/{pha.input-data-type}/
pha.path.neighbors.station-element-data-in = data/ghcnm_v4/{pha.neighbors.input-data-type}/
pha.path.station-element-data-out = output/ghcnm_v4_adjusted/{pha.element}/
pha.path.station-history = data/history/

# === Neighbor Selection Parameters ===
# Reduced for smaller sample
pha.neighbors.distance-neighbor-limit = 20
pha.neighbors.method = first-diffs
pha.neighbors.min-coefficient = 0.0
pha.neighbors.min-station-coverage = 3
pha.neighbors.final-neighbor-limit = 10

# === Changepoint Detection/Adjustment Parameters ===
pha.use-history-files = 0
pha.snht-threshold = 5
pha.bic-penalty = bic
pha.amploc-percent = 92
pha.confirm = 2
pha.adjust.min-length = 18
pha.adjust.min-neighbors = 3
pha.adjust.remove-outliers = true
pha.adjust.window = 0
pha.adjust.filter-method = conf
pha.adjust.est-method = med
pha.remove-insignificant = true
EOF

echo ""
echo "=== Setup Complete! ==="
echo ""
echo "Summary:"
echo "  Region: ${REGION}"
echo "  Stations: ${STATION_COUNT}"
echo "  Element: ${ELEMENT}"
echo "  Inventory: data/ghcnm_${REGION}.inv"
echo "  Data: data/ghcnm_v4/raw/*.raw.${ELEMENT}"
echo "  Config: ghcnm-pha-${REGION}.properties"
echo ""
echo "To run the PHA analysis:"
echo "  ./bin/PHAMain"
echo ""
echo "The program will read configuration from: ghcnm-pha.properties"
echo "Copy your regional config:"
echo "  cp ghcnm-pha-${REGION}.properties ghcnm-pha.properties"
echo "  ./bin/PHAMain"
echo ""
echo "Or set the config file directly (if the program supports it):"
echo "  ./bin/PHAMain ghcnm-pha-${REGION}.properties"
echo ""
