#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: ./quickstart_tob.sh [--data-dir DIR] [--skip-make] [--no-viewer]
                          [--viewer-host HOST] [--viewer-port PORT]

Runs the TOB + PHA quickstart flow:
  1) build binaries (unless --skip-make)
  2) conditionally download input archives (only if remote file is newer)
  3) reconstruct input/output layout
  4) reconstruct history files from QCU/QCF deltas
  5) run TOBMain and PHAMain
  6) launch PHAview (unless --no-viewer)
USAGE
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="${SCRIPT_DIR}"
cd "${REPO_ROOT}"

DATA_DIR="data"
SKIP_MAKE=0
LAUNCH_VIEWER=1
VIEWER_HOST="${VIEWER_HOST:-127.0.0.1}"
VIEWER_PORT="${VIEWER_PORT:-8080}"
NOAA_GHCN_BASE_URL="${NOAA_GHCN_BASE_URL:-https://www.ncei.noaa.gov/pub/data/ghcn/v4}"
NOAA_HOMR_BASE_URL="${NOAA_HOMR_BASE_URL:-https://www.ncei.noaa.gov/access/homr/file}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --data-dir)
      DATA_DIR="${2:-}"
      shift 2
      ;;
    --skip-make)
      SKIP_MAKE=1
      shift
      ;;
    --no-viewer)
      LAUNCH_VIEWER=0
      shift
      ;;
    --viewer-host)
      VIEWER_HOST="${2:-}"
      shift 2
      ;;
    --viewer-port)
      VIEWER_PORT="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ -z "${DATA_DIR}" ]]; then
  echo "--data-dir must not be empty" >&2
  exit 1
fi
if [[ -z "${VIEWER_HOST}" ]]; then
  echo "--viewer-host must not be empty" >&2
  exit 1
fi
if ! [[ "${VIEWER_PORT}" =~ ^[0-9]+$ ]] || (( VIEWER_PORT < 1 || VIEWER_PORT > 65535 )); then
  echo "--viewer-port must be an integer in range 1-65535" >&2
  exit 1
fi

download_if_newer() {
  local url="$1"
  local dest="$2"
  local args=(
    --fail
    --silent
    --show-error
    --location
    --remote-time
    --output "${dest}"
  )
  local before="missing"
  local after

  file_signature() {
    local path="$1"
    if stat -f '%m:%z' "${path}" >/dev/null 2>&1; then
      stat -f '%m:%z' "${path}"
    else
      stat -c '%Y:%s' "${path}"
    fi
  }

  if [[ -f "${dest}" ]]; then
    before="$(file_signature "${dest}")"
    args+=(--time-cond "${dest}")
  fi

  echo "Syncing ${dest}"
  curl "${args[@]}" "${url}"

  after="$(file_signature "${dest}")"
  if [[ "${before}" == "${after}" ]]; then
    echo "Unchanged: ${dest}"
  else
    echo "Updated: ${dest}"
  fi
}

if [[ "${SKIP_MAKE}" -eq 0 ]]; then
  echo "Building binaries with make"
  make
fi

mkdir -p "${DATA_DIR}"

download_if_newer \
  "${NOAA_GHCN_BASE_URL}/ghcnm.tavg.latest.qcu.tar.gz" \
  "${DATA_DIR}/ghcnm.tavg.latest.qcu.tar.gz"
download_if_newer \
  "${NOAA_GHCN_BASE_URL}/ghcnm.tavg.latest.qcf.tar.gz" \
  "${DATA_DIR}/ghcnm.tavg.latest.qcf.tar.gz"
download_if_newer \
  "${NOAA_HOMR_BASE_URL}/phr.txt.zip" \
  "${DATA_DIR}/phr.txt.zip"
download_if_newer \
  "${NOAA_HOMR_BASE_URL}/mshr_enhanced.txt.zip" \
  "${DATA_DIR}/mshr_enhanced.txt.zip"

python3 src/python/qcu_to_inputs.py \
  --qcu-tar "${DATA_DIR}/ghcnm.tavg.latest.qcu.tar.gz" \
  --base "${DATA_DIR}"

python3 src/python/qcf_to_outputs.py \
  --qcf-tar "${DATA_DIR}/ghcnm.tavg.latest.qcf.tar.gz" \
  --base "${DATA_DIR}"

python3 src/python/qcufdelta_to_his.py \
  --inv "${DATA_DIR}/input/station.inv" \
  --qcu-dir "${DATA_DIR}/input/raw/tavg" \
  --qcf-dir "${DATA_DIR}/output/qcf/tavg" \
  --out-history-dir "${DATA_DIR}/input/history" \
  --mshr-zip "${DATA_DIR}/mshr_enhanced.txt.zip" \
  --tob-bin "bin/TOBMain"

bin/TOBMain -p "${DATA_DIR}/tob.properties"
bin/PHAMain -p "${DATA_DIR}/tob.properties"

echo
echo "Quickstart complete."

if [[ "${LAUNCH_VIEWER}" -eq 0 ]]; then
  echo "Viewer launch skipped (--no-viewer)."
  exit 0
fi

VIEWER_DIR="${DATA_DIR}/output/adj/tavg"
VIEWER_REF="${DATA_DIR}/output/qcf/tavg"
VIEWER_REF2="${DATA_DIR}/input/tob/tavg"
VIEWER_INV="${DATA_DIR}/input/station.inv"

echo "Launching viewer at http://${VIEWER_HOST}:${VIEWER_PORT}/"
exec bin/PHAview \
  --dir "${VIEWER_DIR}" \
  --ref "${VIEWER_REF}" \
  --ref2 "${VIEWER_REF2}" \
  --inventory "${VIEWER_INV}" \
  --host "${VIEWER_HOST}" \
  --port "${VIEWER_PORT}"
