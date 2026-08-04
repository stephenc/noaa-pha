# Workflows

## Run the pipeline end to end

`./quickstart_tob.sh` does all of this. The steps below are the same sequence
as separate commands.

### 1. Download the inputs

```bash
mkdir -p ./data
curl -o ./data/ghcnm.tavg.latest.qcu.tar.gz https://www.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tavg.latest.qcu.tar.gz
curl -o ./data/ghcnm.tavg.latest.qcf.tar.gz https://www.ncei.noaa.gov/pub/data/ghcn/v4/ghcnm.tavg.latest.qcf.tar.gz
curl -o ./data/phr.txt.zip https://www.ncei.noaa.gov/access/homr/file/phr.txt.zip
curl -o ./data/mshr_enhanced.txt.zip https://www.ncei.noaa.gov/access/homr/file/mshr_enhanced.txt.zip
```

The `latest` QCU and QCF files change daily. For a fixed vintage, use NOAA's
dated archive, which starts at 2026-04-29:
<https://www.ncei.noaa.gov/data/global-historical-climatology-network-monthly/v4/temperature/archive/>

HOMR does not put a date on `phr.txt.zip` or `mshr_enhanced.txt.zip`. Keep the
copy that you used. `src/python/fetch_homr.py` downloads these two files and
records the URL, the time and the checksum of each one.

### 2. Build the workspace

```bash
python3 src/python/qcu_to_inputs.py \
  --qcu-tar ./data/ghcnm.tavg.latest.qcu.tar.gz --base data

python3 src/python/qcf_to_outputs.py \
  --qcf-tar ./data/ghcnm.tavg.latest.qcf.tar.gz --base data
```

The first command also writes `data/tob.properties` and `data/raw.properties`.
These contain the recovered PHA configuration. `docs/DATA_FORMATS.md` gives the
values and the evidence for them.

### 3. Recover the station histories

NOAA does not publish the station histories (`data/intermediate/history/*.his`).
Recover them with one command:

```bash
uv run python src/python/reconstruct_his.py --base data
```

Build `bin/TOBMain` with `TRIG_BACKEND=llvm-exact` first. The command also
makes the other parts of `data/intermediate/` that `tob.properties` needs.

Use `--hints` to read a hint databank. The option takes one directory, and you
can give it more than once:

```bash
args=(); for d in /path/to/hintstore/*/; do args+=(--hints "$d"); done
uv run python src/python/reconstruct_his.py --base data "${args[@]}" --jobs 11
```

A databank increases the number of months that the recovery can prove.
`src/python/README.md` tells you how to build one, and what the limits are.

### 4. Run TOB and PHA

```bash
bin/TOBMain -p data/tob.properties
bin/PHAMain -p data/tob.properties
```

To run PHA on the raw data, with no TOB stage:

```bash
bin/PHAMain -p data/raw.properties
```

### 5. Compare the output

Both pipelines write to the same directory, thus the same commands apply.

```bash
# against the QCU input, as a sanity check
python3 src/python/compare_dirs.py \
  data/input/raw/tavg data/output/adj/tavg --header

# against the published QCF
python3 src/python/compare_dirs.py \
  data/output/adj/tavg data/output/qcf/tavg --header
```

For a score instead of a difference list, use `src/python/pha_fit_score.py`.

To see the series:

```bash
bin/PHAview \
  --inventory data/intermediate/station.inv \
  --history data/intermediate/history \
  --dir data/output/adj/tavg \
  --ref data/output/qcf/tavg \
  --ref2 data/intermediate/tob/tavg
```

## Run the tests

```bash
make test          # all tests
make unit-test     # PHATestUnits, with build/ghcnm-pha.unit-test.properties
make output-test   # PHATestOutput, with build/ghcnm-pha.test.properties
```

The tests write their logs to `build/`.

For the Python helpers:

```bash
uv run --with pytest python -m pytest src/python/tests
```

## Docker

```bash
docker build -t noaa-ghcnm-v4 .
docker run -it --rm noaa-ghcnm-v4 bash
```

The code is in `/app`. The programs are in `/app/bin`, which is on the `PATH`.
Use volume mounts for the input and output directories.

The `Dockerfile` builds in stages. The final image holds only the programs and
what they need to run. The `builder` stage holds the full build environment.
