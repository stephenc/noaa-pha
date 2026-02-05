# Python Helpers (Project Additions)

These Python scripts are **project-specific helpers** added in this repository.
They are **not** part of the original NOAA source-code tarball. Their purpose is to:

- Prepare input data for the PHA pipeline from the published **QCU** dataset.
- Prepare comparable output data from the published **QCF** dataset.
- Reconstruct station history (`.his`) files using the HOMR PHR report.
- Compare per-station outputs between two directories.

## What Each Script Does

- `phr_to_his.py`
  - Reconstructs station history (`.his`) files from the HOMR PHR fixed-width report.
  - Uses station inventory for lat/lon/elev and outputs one `.his` per station.
  - If MSHR is provided, merges location/elevation/relocation from MSHR and applies a priority order:
    - PHR (if ever present for location fields) > MSHR > inventory > last-known > 0.0 fallback.

- `qcu_to_inputs.py`
  - Reads a GHCN-M v4 **QCU** `tar.gz` and builds the PHA input directory layout.
  - Converts fixed-width `TAVG` `.dat` into per-station files for the PHA pipeline.
  - Writes `tavg.properties` and `tobs.properties` with pipeline settings.

- `qcf_to_outputs.py`
  - Reads a GHCN-M v4 **QCF** `tar.gz` and writes per-station output files.
  - Intended for like-for-like comparisons with PHA outputs.

- `compare_dirs.py`
  - Compares two directories of per-station files.
  - Matches by station ID (first filename segment).
  - Outputs summary stats in Celsius and 30-year bins.

## Data Sources (Official Downloads)

GHCN-M v4 (QCU/QCF) direct download index:
```
https://www.ncei.noaa.gov/pub/data/ghcn/v4/
```

Typical filenames from that index:
```
ghcnm.tavg.latest.qcu.tar.gz
ghcnm.tavg.latest.qcf.tar.gz
```

HOMR PHR (Publication History Report) downloads:
```
https://www.ncei.noaa.gov/access/homr/reports
```

From the PHR section on that page:
```
https://www.ncei.noaa.gov/access/homr/file/phr.txt.zip
https://www.ncei.noaa.gov/access/homr/file/PHR_Table.txt
```

Enhanced MSHR downloads and layout:
```
https://www.ncei.noaa.gov/access/homr/file/mshr_enhanced.txt.zip
https://www.ncei.noaa.gov/access/homr/file/MSHR_Enhanced_Table.txt
```

## Notes

- All parsing is **fixed-width**, per the official dataset layouts.
- Output values from `compare_dirs.py` are in **Celsius** (hundredths scaled to degrees).
