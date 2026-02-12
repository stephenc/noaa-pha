# Go Viewer

## Overview

The Go viewer is a small HTTP app for inspecting and comparing station time series.

It supports:

- a primary data directory (`--dir`, required)
- an optional first reference directory (`--ref`)
- an optional second reference directory (`--ref2`)
- an optional inventory file (`--inventory`) for station names and gridded views

The UI is served at `/` and JSON data is served from `/api/*`.

## What The Viewer Computes

- Mean temperature time series in deg C (`monthly` or `yearly`)
- Count series (`count` mode) for valid monthly values
- Optional differences against a reference (`dir - ref`)
- Optional gridded aggregation (requires `--inventory`)
- Station-level filtering and selection

All station values are read as hundredths of deg C and converted to deg C in outputs.
Missing values are `-9999`.

## Input Expectations

### Station directories (`--dir`, `--ref`, `--ref2`)

- Each file is treated as one station record.
- Station ID is taken from the filename prefix before the first `.`.
- Comparison only occurs for station IDs present in both sides being compared.

The parser expects fixed-width monthly rows with:

- year at columns `13-16`
- 12 monthly blocks of width `9` each (`value[6] + flags[3]`)
- QC uses the second flag character in each monthly block

### Inventory file (`--inventory`)

Optional but required for gridded views.
Expected fixed-width fields:

- station ID: columns `1-11`
- latitude: columns `13-20`
- longitude: columns `22-30`
- station name: columns `39-68` (if present)

## Run

Prerequisite: Go `1.22+` (matches `src/go/go.mod`).

From the repository root:

```bash
go run ./src/go --dir /path/to/primary
```

With one reference and inventory:

```bash
go run ./src/go \
  --dir /path/to/primary \
  --ref /path/to/reference \
  --inventory /path/to/ghcnm.inv
```

With two references:

```bash
go run ./src/go \
  --dir /path/to/primary \
  --ref /path/to/reference1 \
  --ref2 /path/to/reference2 \
  --inventory /path/to/ghcnm.inv
```

Open:

```text
http://127.0.0.1:8080
```

## Flags

- `--dir` (required): primary station directory
- `--ref`: first reference station directory
- `--ref2`: second reference station directory
- `--inventory`: station inventory file
- `--host` (default `127.0.0.1`)
- `--port` (default `8080`)

## UI Quick Guide

- `Ignore QC` / `Include QC`: include only unflagged data or all data
- `Monthly` / `Yearly` / `Count`: aggregation granularity
- `Numerical` / `Gridded`: overall average vs gridded average (inventory required)
- `All` / `Selection`: use all stations or selected stations
- Compare toggles: choose no reference, `ref1`, `ref2`, or both (if provided)
- Station filter supports free text, plus `id:<pattern>` and `name:<pattern>` with `*` wildcard

The page state is mirrored in query parameters, so views can be shared by URL.

## JSON Endpoints

- `GET /api/stations?include_qc=0|1`
- `GET /api/summary?mode=overall|grid&include_qc=0|1`
- `GET /api/series?mode=overall|grid|station&compare=0|1&granularity=monthly|yearly|count&station_id=...&ref=1|2&include_qc=0|1`
- `GET /api/station?id=<station>&compare=0|1&include_qc=0|1`

Responses include `ETag` headers for client-side caching.
