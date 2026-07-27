#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: ./quickstart_tob.sh [--data-dir DIR] [--skip-make] [--no-viewer]
                          [--viewer-host HOST] [--viewer-port PORT]

Runs the TOB + PHA quickstart flow:
  1) build binaries (unless --skip-make); TOB bit-exactness needs
     TRIG_BACKEND=llvm-exact (set when invoking make, or rebuild later)
  2) conditionally download input archives (only if remote file is newer)
  3) reconstruct input/output layout
  4) reconstruct history + provision intermediate/ when history is empty
     or intermediate/ is not ready (requires uv)
  5) run TOBMain and PHAMain
  6) launch PHAview (unless --no-viewer)

Optional after a successful TOBMain run (not part of this script):
  uv run python src/python/verify_his.py --jobs 8
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

# Station history (.his) files are not published by NOAA. Reconstruction
# writes them under intermediate/history and also provisions
# intermediate/station.inv + intermediate/tob/tavg required by tob.properties.
# Re-run when history is empty OR intermediate is not ready (CONUS:
# solver-derived from QCU/QCF residuals; non-CONUS: metadata-derived from
# MSHR/PHR).
history_empty=0
if [[ -z "$(ls -A "${DATA_DIR}/intermediate/history" 2>/dev/null)" ]]; then
  history_empty=1
fi
intermediate_ready=1
if [[ ! -s "${DATA_DIR}/intermediate/station.inv" ]] || \
   [[ ! -d "${DATA_DIR}/intermediate/tob/tavg" ]]; then
  intermediate_ready=0
fi
if [[ "${history_empty}" -eq 1 || "${intermediate_ready}" -eq 0 ]]; then
  if command -v uv >/dev/null 2>&1; then
    reconstruct_args=(--base "${DATA_DIR}")
    # History already present: skip stations with cached solutions so we only
    # re-provision intermediate/ (and fill any missing solutions) rather than
    # re-solving the full inventory.
    if [[ "${history_empty}" -eq 0 ]]; then
      reconstruct_args+=(--skip-existing)
    fi
    echo "Reconstructing station histories / intermediate into ${DATA_DIR} (may take 15-20 min)..."
    uv run python src/python/reconstruct_his.py "${reconstruct_args[@]}"
  else
    echo "ERROR: history/intermediate not ready under ${DATA_DIR} and 'uv' is not installed." >&2
    echo "Install uv (https://docs.astral.sh/uv/) and run:" >&2
    echo "  uv run python src/python/reconstruct_his.py --base ${DATA_DIR}" >&2
    exit 1
  fi
fi

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
VIEWER_REF2="${DATA_DIR}/intermediate/tob/tavg"
VIEWER_HIS="${DATA_DIR}/intermediate/history"
VIEWER_INV="${DATA_DIR}/intermediate/station.inv"

echo "Launching viewer at http://${VIEWER_HOST}:${VIEWER_PORT}/"
exec bin/PHAview \
  --dir "${VIEWER_DIR}" \
  --ref "${VIEWER_REF}" \
  --ref2 "${VIEWER_REF2}" \
  --history "${VIEWER_HIS}" \
  --inventory "${VIEWER_INV}" \
  --host "${VIEWER_HOST}" \
  --port "${VIEWER_PORT}"
